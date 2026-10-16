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
        ''' Resolve an assembler address to the rope-image bank and the AGC bank
        ''' number used for word accounting.  The rope image uses the AGC's own
        ''' bank numbering, which is what AgcMemory also decodes to.
        ''' </summary>
        Public Shared Sub ResolveBank(address As AgcAddress, ByRef physical As Integer, ByRef logical As Integer)
            physical = -1
            logical = -1
            If Not address.IsFixed OrElse address.Invalid Then Return

            Dim sreg As Integer = address.SReg And AgcWord.Mask15
            Dim moduleNumber As Integer
            Dim half As Integer

            If address.Unbanked Then
                If sreg < 2048 Then Return                       ' not a fixed address
                If sreg < 4096 Then
                    ' 04000..07777: banks 02 and 03 of module 2.
                    moduleNumber = 2
                    half = (sreg - 2048) \ 1024
                    logical = 2 + half
                Else
                    ' 010000..017777 is module 2, 030000.. is the super-bank module.
                    Dim rel As Integer = sreg - 4096
                    moduleNumber = 2 + rel \ 2048
                    half = (rel Mod 2048) \ 1024
                    logical = If(moduleNumber = 2, 2 + half, address.FB)
                End If
            Else
                ' Banked: the module comes from the FB field (030..043 are the
                ' super-bank module) and the half from the 01000 bit of SReg.
                moduleNumber = If(address.Super, address.FB - 8, address.FB)
                half = (sreg - 2048) \ 1024
                logical = address.FB
            End If

            physical = logical
        End Sub

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

        ' Set while the interpretive instruction being assembled is a member of the
        ' STORE family, whose operand word is a complemented store code rather than
        ' a plain address.
        Private _interpIsStore As Boolean
        Private _interpStoreNumber As Integer

        Private _emittedWords As Integer

        ''' <summary>
        ''' An equate whose right-hand side could not be evaluated when the line was
        ''' reached, because it names a symbol defined further down the file.
        '''
        ''' The AGC sources rely on this heavily: ERASABLE_ASSIGNMENTS.agc defines
        ''' "FLAGWRD5 = STATE +5" more than six hundred lines before "STATE ERASE".
        ''' Patching these up after the pass completes is what makes such references
        ''' resolve to addresses instead of stale constants.
        ''' </summary>
        Private Structure DeferredEquate
            Public Label As String
            Public Operand As String
            Public IsMinus As Boolean
            Public IsEcadr As Boolean
            Public File As String
            Public Line As Integer
        End Structure

        Private ReadOnly _deferredEquates As New List(Of DeferredEquate)()

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

            Dim reader As New AgcSourceReader(SourceDirectory, Me.Diagnostics)
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
            ResolveDeferredEquates()

            ' Equates that named a symbol defined further down the file could only
            ' be evaluated after the first sweep, so run the sweep again now that
            ' they hold real values.  Without this, a use site in pass 2 still sees
            ' the placeholder the equate had while its operand was unknown.
            ResetPass()
            RunPass()
            ResolveDeferredEquates()
            Diagnostics.Suppress = False

            ' Pass 2 has every symbol available, so this is where real problems
            ' are reported.
            _pass = 2
            Image.Clear()
            ResetPass()
            RunPass()
            ResolveDeferredEquates()

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

        ''' <summary>Development aid: print each encoded instruction.</summary>
        Public Property _debugEncoding As Boolean = False

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
            StoreWordAt(target, word)
            _emittedWords += 1
        End Sub

        Private Sub Advance(count As Integer)
            CreditWords(_pc, count)
            _pc = AgcCodec.IncPc(_pc, count)
        End Sub

        Private Sub StoreWordAt(address As AgcAddress, word As Integer)
            ' The AGC instruction word is 16 bits wide (15 data bits plus the
            ' parity bit), so bit 16 must be preserved for the CPU decoder.
            word = word And AgcWord.Mask17
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
                Dim physical As Integer = -1
                Dim logical As Integer = -1
                ResolveBank(address, physical, logical)
                If physical >= 0 AndAlso physical < AgcRopeImage.FixedBankCount Then
                    Image.SetFixed(physical, 2048 + (sreg Mod 1024), word)
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
        ''' <summary>
        ''' Remember an equate that could not be evaluated yet.  The operand names a
        ''' symbol that is defined further down the file.
        ''' </summary>
        Private Sub DeferEquate(line As AgcLine, operand As String, isMinus As Boolean, isEcadr As Boolean)
            If String.IsNullOrEmpty(line.Label) Then Return
            If _debugEncoding Then
                Console.WriteLine("DEFER pass=" & _pass.ToString() & " " & line.Label &
                                  " = " & operand & " (" & System.IO.Path.GetFileName(line.File) &
                                  ":" & line.LineNumber.ToString() & ")")
            End If
            Dim d As New DeferredEquate()
            d.Label = line.Label
            d.Operand = operand
            d.IsMinus = isMinus
            d.IsEcadr = isEcadr
            d.File = line.File
            d.Line = line.LineNumber
            _deferredEquates.Add(d)
        End Sub

        ''' <summary>
        ''' Retry equates whose operands were not known when they were first seen.
        ''' Repeat until no further progress, so that a chain of dependent equates
        ''' resolves regardless of the order they appear in.
        ''' </summary>
        Private Sub ResolveDeferredEquates()
            If _deferredEquates.Count = 0 Then Return

            Dim remaining As New List(Of DeferredEquate)(_deferredEquates)
            _deferredEquates.Clear()

            Dim progress As Boolean = True
            Dim passes As Integer = 0
            While progress AndAlso remaining.Count > 0 AndAlso passes < 16
                progress = False
                passes += 1
                Dim stillStuck As New List(Of DeferredEquate)()

                For Each d As DeferredEquate In remaining
                    Dim name As String = d.Operand
                    Dim off As Integer = 0
                    Dim numeric As Boolean = False
                    Dim a As AgcAddress = ResolveOperand(name, off, numeric)
                    If a.Invalid Then
                        ' Still unknown; try again on the next sweep.
                        stillStuck.Add(d)
                        Continue For
                    End If

                    If d.IsEcadr Then
                        Symbols.Define(d.Label, AgcAddress.Constant(EcadrOf(a)), True, d.File, d.Line)
                    ElseIf d.IsMinus Then
                        Symbols.Define(d.Label, AgcAddress.Constant(a.SReg - _pc.SReg), True, d.File, d.Line)
                    Else
                        If numeric Then a.IsConstant = True
                        Symbols.Define(d.Label, a, True, d.File, d.Line)
                    End If
                    progress = True
                Next

                remaining = stillStuck
            End While

            ' Anything still unresolved is a genuine error; report it once.
            For Each d As DeferredEquate In remaining
                If _pass = 2 Then
                    Diagnostics.Fatal(d.File, d.Line,
                                      "Equate operand '" & d.Operand & "' is undefined.")
                End If
            Next
            _deferredEquates.Clear()
        End Sub

        Private Sub CreditWords(address As AgcAddress, count As Integer)
            If Not address.IsFixed OrElse address.Invalid Then Return
            Dim sreg As Integer = address.SReg And AgcWord.Mask15
            Dim physical As Integer = -1
            Dim logical As Integer = -1
            ResolveBank(address, physical, logical)
            If physical < 0 OrElse logical < 0 Then Return
            If physical >= AgcRopeImage.FixedBankCount Then Return
            Dim used As Integer = (sreg Mod 1024) + count
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

        ''' <summary>
        ''' Look up a symbol's pseudo address, or -1 when it is not defined.  This
        ''' is the value a running program uses to find its own entry points.
        ''' </summary>
        Public Function Lookup(name As String) As Integer
            Dim sym As AgcSymbol = Nothing
            If Symbols.TryGetValue(name, sym) Then Return sym.Address.PseudoValue
            Return -1
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
        '''
        ''' The offset may be separated by whitespace, which the AGC sources use
        ''' heavily ("FLAGWRD5 = STATE +5").  Treating the space as part of the name
        ''' makes the lookup fail and the equate silently becomes a constant, so the
        ''' whitespace is skipped here and trailing blanks are trimmed.
        ''' </summary>
        Private Sub SplitSymbolAndOffset(ByRef name As String, ByRef offset As Integer)
            offset = 0
            If String.IsNullOrEmpty(name) Then Return
            Dim s As String = name.Trim()
            If s.Length = 0 Then
                name = ""
                Return
            End If

            Dim i As Integer = 1
            While i < s.Length
                Dim c As Char = s(i)
                If c = "+"c OrElse c = "-"c Then
                    ' A sign introduces the offset; whitespace before it is padding
                    ' rather than part of the symbol name.
                    Dim tail As String = s.Substring(i).Trim()
                    Dim v As Integer
                    If AgcWord.TryParseInteger(tail, v) Then
                        offset = v
                        name = s.Substring(0, i).Trim()
                        Return
                    End If
                    ' Not an offset after all (for example a name containing a sign),
                    ' so keep scanning.
                End If
                i += 1
            End While

            ' No offset: the whole trimmed text is the name.
            name = s
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
            If sym.Super Then
                ' A super-bank symbol already carries its bank and sub-address
                ' reliably, and its PseudoValue is encoded 02000 below the flat form
                ' that PseudoToAddress decodes for those banks.  Rather than round
                ' trip through the flat space and land on an unrelated low bank,
                ' rebuild the address from the symbol's own fields.
                result = sym
                result.SReg = sym.SReg + symOffset + offset
                result.IsConstant = False
                Return result
            End If
            result = AgcCodec.PseudoToAddress(total)
            If result.Invalid Then
                If _debugEncoding Then
                    Console.WriteLine("UNRESOLVED name=[" & name & "] symName=[" & symName &
                                      "] sym=" & sym.ToString() &
                                      " pseudo=" & sym.PseudoValue.ToString() &
                                      " symOffset=" & symOffset.ToString() &
                                      " offset=" & offset.ToString() &
                                      " total=" & total.ToString())
                End If
                result = AgcAddress.Constant(total)
            Else
                ' A stored symbol always denotes memory.  The "this is also a
                ' plain number" interpretation is only kept for operands the
                ' source wrote as literals, so that "CAF 0" still works.
                result.IsConstant = False
            End If
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
