' ============================================================================
'  Assembler.vb -- the AGC (yaYUL-compatible) two-pass assembler.
'
'  Produces an AgcRopeImage from the transcribed Apollo AGC sources.  Address
'  handling, bank accounting and opcode encoding follow yaYUL (the assembler
'  used to build the virtualagc rope images) so that the resulting rope can be
'  compared word for word against the published listings.
'
'  Two passes:
'    pass 1 -- build the symbol table, learn how much of each bank is used
'    pass 2 -- re-evaluate everything with all symbols known and emit the rope
' ============================================================================

Imports System
Imports System.Collections.Generic
Imports System.IO
Imports System.Text

Namespace Agc

    Public Class AgcAssembler

        ' ------------------------------------------------------------------
        ' Configuration
        ' ------------------------------------------------------------------

        Public ReadOnly Diagnostics As New AgcDiagnostics()
        Public ReadOnly Symbols As New AgcSymbolTable()
        Public ReadOnly Image As New AgcRopeImage()

        Public Property SourceDirectory As String
        Public Property MainFile As String = "MAIN.agc"
        Public Property ProgramName As String = "AGC"
        Public Property Verbose As Boolean

        ' ------------------------------------------------------------------
        ' State
        ' ------------------------------------------------------------------

        Private _pass As Integer
        Private _pc As AgcAddress
        Private _ebank As Integer
        Private _sbank As Integer = 3
        Private _ebankOneShot As Boolean

        ''' <summary>
        ''' Map an AGC bank number into the index used for the per-bank word
        ''' counters.  Banks 00 and 01 are the two halves of the unswitched
        ''' "fixed-fixed" area and share physical storage with banks 02 and 03:
        ''' "BANK 0" writes 000000..003777 and "BANK 2" writes 010000..013777,
        ''' but both end up in bank 02 of the rope image.
        ''' </summary>
        Public Shared Function PhysicalBank(bank As Integer) As Integer
            If bank = 0 OrElse bank = 2 Then Return 2
            If bank = 1 OrElse bank = 3 Then Return 3
            Return bank
        End Function

        ''' <summary>
        ''' The AGC bank number a location belongs to.  Banks 00/01 and 02/03
        ''' share physical rope banks but are allocated independently: within
        ''' physical bank 2 the low 1024 words are "BANK 0" (written
        ''' 000000..003777) and the high 1024 are "BANK 2" (written
        ''' 010000..013777); physical bank 3 splits into "BANK 1" and "BANK 3"
        ''' the same way.
        ''' </summary>
        Public Shared Function LogicalBank(physicalBank As Integer, offset As Integer) As Integer
            If physicalBank = 2 Then
                If offset < 1024 Then Return 0
                Return 2
            End If
            If physicalBank = 3 Then
                If offset < 1024 Then Return 1
                Return 3
            End If
            Return physicalBank
        End Function

        ''' <summary>
        ''' High-water mark of words used in each fixed bank, as in yaYUL's
        ''' UsedInBank[] table.  "BANK n" places the location counter at the
        ''' first unused word, so this must be a maximum, not a running total.
        ''' </summary>
        Private ReadOnly _usedInBank(AgcRopeImage.FixedBankCount - 1) As Integer
        Private _pendingExtend As Boolean
        Private _lines As List(Of AgcLine)
        Private _currentLine As AgcLine

        ' Interpretive operand queue.
        Private _interpOperandsPending As Integer
        Private ReadOnly _interpNnnn As New List(Of Integer)()
        Private ReadOnly _interpSwitchInc As New List(Of Integer)()
        Private ReadOnly _interpSwitchInvert As New List(Of Integer)()
        Private _interpIndex As Integer

        Private _emittedWords As Integer
        Private _assembledLines As Integer

        Public ReadOnly Property UsedInBank As Integer()
            Get
                Return _usedInBank
            End Get
        End Property

        Public ReadOnly Property EmittedWords As Integer
            Get
                Return _emittedWords
            End Get
        End Property

        Public ReadOnly Property TotalAssembledLines As Integer
            Get
                Return _assembledLines
            End Get
        End Property

        Public ReadOnly Property ProgramCounter As AgcAddress
            Get
                Return _pc
            End Get
        End Property

        ' ------------------------------------------------------------------
        ' Assembly driver
        ' ------------------------------------------------------------------

        Public Function Assemble() As Boolean
            If String.IsNullOrEmpty(SourceDirectory) Then
                Throw New InvalidOperationException("SourceDirectory must be set.")
            End If

            Dim reader As New AgcSourceReader(SourceDirectory, Diagnostics)
            _lines = reader.Read(Path.Combine(SourceDirectory, MainFile))
            If _lines.Count = 0 Then
                Diagnostics.Fatal(MainFile, 0, "No source lines were read.")
                Return False
            End If
            If Verbose Then Diagnostics.Info(MainFile, 0, _lines.Count.ToString() & " source lines read.")

            Dim baseline As Integer = Diagnostics.ErrorCount

            ' Pass 1 builds the symbol table.  Forward references are expected to
            ' be unresolved at this point, so pass-1 diagnostics are suppressed.
            _pass = 1
            Diagnostics.Suppress = True
            ResetPass()
            RunPass()
            Diagnostics.Suppress = False

            ' Pass 2 has every symbol available, so this is where real problems
            ' are reported.
            _pass = 2
            Image.Clear()
            ResetPass()
            RunPass()

            ReportUndefinedSymbols()

            Image.ComputeChecksums()
            Return Diagnostics.ErrorCount = baseline
        End Function

        ''' <summary>
        ''' Report symbols that were referenced but never defined.  This replaces
        ''' the per-line "not resolved" complaints, which fire on legitimate
        ''' forward references during pass 1.
        ''' </summary>
        Private Sub ReportUndefinedSymbols()
            For Each sym As AgcSymbol In Symbols.ReferencedButUndefined()
                Diagnostics.[Error](MainFile, 0, "Undefined symbol '" & sym.Name & "'.")
            Next
        End Sub

        Private Sub ResetPass()
            Array.Clear(_usedInBank, 0, _usedInBank.Length)
            _ebank = 0
            _sbank = 3
            _ebankOneShot = False
            _pc = AgcAddress.UnswitchedErasable(0)
            _pendingExtend = False
            _interpOperandsPending = 0
            _interpNnnn.Clear()
            _interpSwitchInc.Clear()
            _interpSwitchInvert.Clear()
            _emittedWords = 0
            _assembledLines = 0
        End Sub

        Public Property _traceLines As Integer = 0

        Private Sub RunPass()
            For Each line As AgcLine In _lines
                _currentLine = line
                ProcessLine(line)
            Next
            If _interpOperandsPending > 0 Then
                Diagnostics.Fatal(_currentLine.File, _currentLine.LineNumber,
                                  "Missing interpretive operands at end of assembly.")
                _interpOperandsPending = 0
            End If
        End Sub

        ' ------------------------------------------------------------------
        ' Dispatch
        ' ------------------------------------------------------------------

        Private Sub ProcessLine(line As AgcLine)
            _assembledLines += 1

            If _interpOperandsPending > 0 Then
                ProcessInterpretiveOperandLine(line)
                Return
            End If

            Dim op As String = line.Opcode

            If String.IsNullOrEmpty(op) Then
                DefineLabel(line, _pc, False)
                If AgcSourceReader.IsRelativeLabel(line.Label) Then
                    _pc = AgcCodec.IncPc(_pc, ParseRelativeAmount(line.Label))
                End If
                Return
            End If

            If AgcSourceReader.IsRelativeLabel(op) Then
                DefineLabel(line, _pc, False)
                _pc = AgcCodec.IncPc(_pc, ParseRelativeAmount(op))
                Return
            End If

            If IsNativePseudoOp(op) OrElse IsDataOpcode(op) Then
                Dim isEquate As Boolean = (op = "EQUALS" OrElse op = "=" OrElse op = "=MINUS" _
                                           OrElse op = "=ECADR" OrElse op = "CHECK=")
                DefineLabel(line, _pc, isEquate)
                ProcessPseudoOp(line)
                Return
            End If

            Dim aliasEntry As AgcOpcode = Nothing
            If AgcOpcodeTable.TryGetBasic(op, aliasEntry) AndAlso (aliasEntry.Flags And OperandFlags.IsAlias) <> 0 Then
                DefineLabel(line, _pc, False)
                ProcessInstruction(line, aliasEntry.AliasOperator, aliasEntry.AliasOperand, False)
                Return
            End If

            Dim interp As AgcOpcode = Nothing
            If AgcOpcodeTable.TryGetInterpretive(op, interp) Then
                DefineLabel(line, _pc, False)
                ProcessInterpretiveOpcode(line, interp)
                Return
            End If

            Dim basic As AgcOpcode = Nothing
            If AgcOpcodeTable.TryGetBasic(op, basic) Then
                DefineLabel(line, _pc, False)
                ProcessInstruction(line, op, line.Operand, False)
                Return
            End If

            Dim numOp As Integer
            If AgcWord.TryParseInteger(op, numOp) AndAlso numOp >= 0 AndAlso numOp <= 7 Then
                DefineLabel(line, _pc, False)
                ProcessRawNumericOpcode(line, numOp)
                Return
            End If

            Diagnostics.Fatal(line.File, line.LineNumber, "Unrecognized opcode/pseudo-op '" & op & "'. [raw=[" & line.RawText.Replace(Chr(9), "<T>") & "] label=[" & line.Label & "] operand=[" & line.Operand & "] native=" & IsNativePseudoOp(op).ToString() & "]")
            DefineLabel(line, _pc, False)
            EmitAtCurrent(0)
            Advance(1)
        End Sub

        Private Shared Function IsNativePseudoOp(op As String) As Boolean
            Select Case op
                Case "SETLOC", "LOC", "BANK", "BLOCK", "EBANK=", "SBANK=", "EQUALS", "=", "=MINUS",
                     "=ECADR", "CHECK=", "ERASE", "COUNT", "COUNT*", "BNKSUM", "MEMORY", "SUBRO",
                     "SECSIZ", "OCT", "OCTAL", "DEC", "DEC*", "2DEC", "2DEC*", "2OCT", "MM",
                     "VN", "NV", "ADRES", "CADR", "FCADR", "2FCADR", "ECADR", "GENADR", "REMADR",
                     "DNPTR", "-DNPTR", "BBCON", "BBCON*", "2CADR", "2BCADR", "XCADR",
                     "1DNADR", "2DNADR", "3DNADR", "4DNADR", "5DNADR", "6DNADR",
                     "-1DNADR", "-2DNADR", "-3DNADR", "-4DNADR", "-5DNADR", "-6DNADR",
                     "-GENADR", "-2CADR", "-DNCHAN", "DNCHAN", "-CCS"
                    Return True
                Case Else
                    Return False
            End Select
        End Function

        Private Shared Function IsDataOpcode(op As String) As Boolean
            Return False
        End Function

        ' ------------------------------------------------------------------
        ' Labels
        ' ------------------------------------------------------------------

        Private Sub DefineLabel(line As AgcLine, address As AgcAddress, isEquate As Boolean)
            If String.IsNullOrEmpty(line.Label) Then Return
            Dim name As String = line.Label
            If AgcSourceReader.IsRelativeLabel(name) Then Return
            If Not IsValidSymbolName(name) Then Return
            Symbols.Define(name, address, isEquate, line.File, line.LineNumber)
        End Sub

        Private Shared Function IsValidSymbolName(name As String) As Boolean
            If String.IsNullOrEmpty(name) Then Return False
            Dim c As Char = name(0)
            If Char.IsLetter(c) Then Return True
            ' The AGC sources use several punctuation-leading tag styles:
            '   /TAG, .TAG, $TAG      ordinary tags
            '   +TAG, -TAG            complement tags (e.g. -CCS)
            '   1/PIPADT, 2DEC        tags that start with a digit but contain
            '                         a letter or punctuation
            If c = "/"c OrElse c = "."c OrElse c = "$"c OrElse c = "+"c OrElse c = "-"c Then Return True
            If Char.IsDigit(c) Then
                For Each ch As Char In name
                    If Not Char.IsDigit(ch) Then Return True
                Next
            End If
            Return False
        End Function

        ' ------------------------------------------------------------------
        ' Emission
        ' ------------------------------------------------------------------

        ''' <summary>
        ''' Store words starting at the current location, then advance past them.
        ''' </summary>
        Private Sub EmitAndAdvance(words As IList(Of Integer))
            Dim start As AgcAddress = _pc
            For i As Integer = 0 To words.Count - 1
                If _pass = 2 Then StoreWordAt(AgcCodec.IncPc(start, i), words(i))
                _emittedWords += 1
            Next
            CreditWords(start, words.Count)
            _pc = AgcCodec.IncPc(start, words.Count)
        End Sub

        ''' <summary>Write word <paramref name="index"/> of a multi-word item at the current location.</summary>
        Private Sub EmitAtCurrent(word As Integer, Optional index As Integer = 0)
            If _pass <> 2 Then Return
            Dim target As AgcAddress = AgcCodec.IncPc(_pc, index)
            If _traceLines > 0 Then
                _traceLines -= 1
                Console.WriteLine("EMIT " & AgcWord.ToOctal(word) & " at " & target.ToString() &
                                  " kind=" & target.Kind.ToString() & " banked=" & target.Banked.ToString() &
                                  " from " & _currentLine.File.Substring(_currentLine.File.LastIndexOf("\"c) + 1) &
                                  ":" & _currentLine.LineNumber.ToString() &
                                  " op=" & _currentLine.Opcode)
            End If
            StoreWordAt(target, word)
            _emittedWords += 1
        End Sub

        Private Sub Advance(count As Integer)
            CreditWords(_pc, count)
            _pc = AgcCodec.IncPc(_pc, count)
        End Sub

        Private Sub StoreWordAt(address As AgcAddress, word As Integer)
            word = word And AgcWord.Mask15
            If address.IsErasable Then
                Dim sreg As Integer = address.SReg And AgcWord.Mask15
                Dim bank As Integer
                Dim off As Integer
                If address.Unbanked Then
                    bank = sreg \ AgcRopeImage.ErasableBankWords
                    off = sreg Mod AgcRopeImage.ErasableBankWords
                Else
                    bank = address.EB
                    off = (sreg - &H1400) + &H100
                End If
                If bank >= 0 AndAlso bank < AgcRopeImage.ErasableBankCount AndAlso
                   off >= 0 AndAlso off < AgcRopeImage.ErasableBankWords Then
                    Image.Erasable(bank, off) = word
                End If
            ElseIf address.IsFixed Then
                Dim sreg As Integer = address.SReg And AgcWord.Mask15
                Dim bank As Integer
                If address.Unbanked Then
                    ' The unswitched fixed window holds banks 02 and 03.  It is
                    ' written either as 010000..017777 or as 04000..07777; both
                    ' forms address the same two banks.
                    If sreg >= 4096 Then
                        bank = 2 + (sreg - 4096) \ 1024
                    Else
                        bank = 2 + (sreg - 2048) \ 1024
                    End If
                Else
                    bank = address.FB + 8 * If(address.Super, 1, 0)
                End If
                If bank >= 0 AndAlso bank < AgcRopeImage.FixedBankCount Then
                    Image.SetFixed(bank, sreg, word)
                End If
            End If
        End Sub

        ''' <summary>
        ''' Update the per-bank high-water marks (yaYUL UpdateBankCounts).  The
        ''' stored value is the number of words in use in the AGC bank, i.e. the
        ''' highest offset reached plus one.
        ''' </summary>
        Private Sub UpdateBankCounts()
            Dim pc As AgcAddress = _pc
            If pc.Invalid OrElse Not pc.IsAddress OrElse Not pc.IsFixed OrElse pc.Overflow Then Return
            CreditWords(pc, 0)
        End Sub

        ''' <summary>
        ''' Credit words to the AGC bank that contains <paramref name="address"/>,
        ''' so that "BANK n" and "BLOCK n" place the following code after anything
        ''' already assembled in that region.  Banks 00/01 and 02/03 share physical
        ''' rope banks but are allocated independently, so the AGC bank number is
        ''' used as the counter index.
        ''' </summary>
        Private Sub CreditWords(address As AgcAddress, count As Integer)
            If Not address.IsFixed OrElse address.Invalid Then Return
            Dim sreg As Integer = address.SReg And AgcWord.Mask15
            Dim physical As Integer
            Dim offset As Integer
            If address.Unbanked Then
                If sreg >= 4096 Then
                    physical = 2 + (sreg - 4096) \ 1024
                    offset = (sreg - 4096) Mod 1024
                ElseIf sreg >= 2048 Then
                    physical = 2 + (sreg - 2048) \ 1024
                    offset = (sreg - 2048) Mod 1024
                Else
                    Return
                End If
            Else
                physical = address.FB + 8 * If(address.Super, 1, 0)
                offset = sreg - 2048
            End If
            If physical < 0 OrElse physical >= AgcRopeImage.FixedBankCount Then Return
            Dim logical As Integer = LogicalBank(physical, offset)
            Dim used As Integer = (offset Mod 1024) + count
            If used < 0 Then used = 0
            If used > AgcRopeImage.FixedBankWords Then used = AgcRopeImage.FixedBankWords
            If used > _usedInBank(logical) Then _usedInBank(logical) = used
        End Sub

        ''' <summary>Number of words already assembled into an AGC bank.</summary>
        Public Function BankUsage(logicalBank As Integer) As Integer
            If logicalBank < 0 OrElse logicalBank >= AgcRopeImage.FixedBankCount Then Return 0
            Return _usedInBank(logicalBank)
        End Function

        Private Function BankUsed(bank As Integer) As Integer
            If bank < 0 OrElse bank >= AgcRopeImage.FixedBankCount Then Return 0
            Return _usedInBank(bank)
        End Function

        Private Function ParseRelativeAmount(text As String) As Integer
            Dim v As Integer
            If AgcWord.TryParseInteger(text, v) Then Return v
            Return 0
        End Function

        ' ------------------------------------------------------------------
        ' Symbol / operand resolution
        ' ------------------------------------------------------------------

        Private Function LookupSymbol(name As String) As AgcAddress
            Dim sym As AgcSymbol = Nothing
            If Symbols.TryGetValue(name, sym) Then
                sym.ReferenceCount += 1
                Return sym.Address
            End If
            Symbols.NoteReference(name)
            Dim bad As New AgcAddress()
            bad.Invalid = True
            Return bad
        End Function

        ''' <summary>
        ''' Split "NAME+3" / "NAME - 2" / "NAME" / "+3" / "-2" into a symbol name
        ''' and a numeric offset.
        ''' </summary>
        Private Sub SplitSymbolAndOffset(ByRef name As String, ByRef offset As Integer)
            offset = 0
            If String.IsNullOrEmpty(name) Then Return
            Dim s As String = name
            Dim i As Integer = 1
            While i < s.Length
                Dim c As Char = s(i)
                If c = "+"c OrElse c = "-"c Then Exit While
                i += 1
            End While
            If i >= s.Length Then Return
            Dim tail As String = s.Substring(i)
            Dim v As Integer
            If AgcWord.TryParseInteger(tail, v) Then
                offset = v
                name = s.Substring(0, i)
            End If
        End Sub

        ''' <summary>
        ''' Resolve an operand field to an address or constant.  Mirrors
        ''' FetchSymbolPlusOffset() plus the number handling in the parsers.
        ''' </summary>
        Private Function ResolveOperand(ByRef name As String, ByRef offset As Integer,
                                        ByRef wasNumeric As Boolean) As AgcAddress
            wasNumeric = False
            Dim result As AgcAddress

            If String.IsNullOrEmpty(name) Then
                Return _pc
            End If

            ' "$n" forces a literal number (used by the EXTEND/INHINT/RELINT aliases).
            If name(0) = "$"c Then
                Dim v As Integer
                If AgcWord.TryParseInteger(name.Substring(1), v) Then
                    wasNumeric = True
                    result = AgcAddress.Constant(v)
                    Return result
                End If
            End If

            If name(0) = "+"c OrElse name(0) = "-"c Then
                Dim v As Integer
                If AgcWord.TryParseInteger(name, v) Then
                    wasNumeric = True
                    Return AgcCodec.IncPc(_pc, v)
                End If
            End If

            Dim num As Integer
            If AgcWord.TryParseInteger(name, num) Then
                wasNumeric = True
                If offset <> 0 Then
                    Dim combined As Integer = num + offset
                    result = AgcCodec.PseudoToAddress(combined)
                    If result.Invalid Then result = AgcAddress.Constant(combined)
                    Return result
                End If
                If num >= 0 AndAlso num <= &H4FFF Then
                    result = AgcCodec.PseudoToAddress(num)
                    ' A number in the erasable/fixed range is also usable as a
                    ' plain constant; keep both interpretations available.
                    If Not result.Invalid Then
                        result.IsConstant = True
                        Return result
                    End If
                End If
                Return AgcAddress.Constant(num)
            End If

            Dim symName As String = name
            Dim symOffset As Integer = 0
            SplitSymbolAndOffset(symName, symOffset)
            Dim sym As AgcAddress = LookupSymbol(symName)
            If sym.Invalid Then
                Dim bad As New AgcAddress()
                bad.Invalid = True
                Return bad
            End If
            Dim total As Integer = sym.PseudoValue + symOffset + offset
            result = AgcCodec.PseudoToAddress(total)
            If result.Invalid Then result = AgcAddress.Constant(total)
            Return result
        End Function

        ''' <summary>Resolve just the address part of an operand, without modifiers.</summary>
        Private Function ResolveOperandSimple(text As String, ByRef ok As Boolean) As AgcAddress
            Dim name As String = text
            Dim offset As Integer = 0
            Dim numeric As Boolean = False
            Dim a As AgcAddress = ResolveOperand(name, offset, numeric)
            ok = Not a.Invalid
            Return a
        End Function

    End Class

End Namespace
