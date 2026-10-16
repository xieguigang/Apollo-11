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
        ''' number used for word accounting.  The rope image indexes fixed memory
        ''' the way the hardware does, so the AGC bank number is used directly
        ''' except for the super-bank module (see RopeBankOf).
        ''' </summary>
        Public Shared Sub ResolveBank(address As AgcAddress, ByRef physical As Integer, ByRef logical As Integer)
            Dim agcBank As Integer = -1
            Dim offset As Integer = -1
            ResolveFixed(address, agcBank, offset)
            If agcBank < 0 Then
                physical = -1
                logical = -1
                Return
            End If
            logical = agcBank
            physical = RopeBankOf(agcBank)
        End Sub

        ''' <summary>
        ''' Check the address model's round trips.  The rope image is addressed by
        ''' AGC bank and 2K offset, and three different routes lead to it:
        '''
        '''     AgcCodec.PseudoToAddress(pseudo)        (an assembler address)
        '''     AgcAddress.PseudoValue                  (back to the flat form)
        '''     ResolveFixed(address) -> (bank, offset) (the rope image)
        '''
        ''' They must agree.  Returns a list of complaints; an empty list means the
        ''' model is self-consistent over the whole fixed pseudo space.
        ''' </summary>
        Public Function AuditAddressModel() As List(Of String)
            Dim problems As New List(Of String)()

            ' One representative word from every 2K bank of the fixed space.  The
            ' fixed-fixed pair's lower half (00000..03777) is addressed as banks 00
            ' and 01 and also lands in bank 02, so the round trip through PseudoValue
            ' canonicalises it to the 04000 form; those probes are checked against
            ' the canonical address instead.
            Dim probes As New List(Of Integer)()
            For b As Integer = 2 To 35
                If b = 2 OrElse b = 3 Then
                    ' The fixed-fixed pair: probe both of its 1K halves, which the
                    ' sources address as 04000 and 06000 (and as 00000/02000).
                    probes.Add(&H4000 + ((b - 2) * 1024))
                    probes.Add(&H4000 + ((b - 2) * 1024) + 1023)
                Else
                    probes.Add(BankPseudoBase(b))
                    probes.Add(BankPseudoBase(b) + 1024)
                    probes.Add(BankPseudoBase(b) + 2047)
                End If
            Next
            probes.Add(&H5000)
            probes.Add(&H7777)

            For Each pseudo As Integer In probes
                If pseudo < 0 OrElse pseudo > AgcWord.Mask15 Then Continue For
                Dim a As AgcAddress = AgcCodec.PseudoToAddress(pseudo)
                If Not a.IsFixed Then
                    problems.Add("pseudo " & AgcWord.ToOctal(pseudo) & " did not resolve to fixed memory")
                    Continue For
                End If
                Dim back As Integer = a.PseudoValue
                If back <> pseudo Then
                    problems.Add("pseudo " & AgcWord.ToOctal(pseudo) & " -> " & a.ToString() &
                                 " -> pseudo " & AgcWord.ToOctal(back))
                    Continue For
                End If
                Dim bank As Integer = -1
                Dim offset As Integer = -1
                ResolveFixed(a, bank, offset)
                Dim expectBank As Integer = -1
                Dim expectOffset As Integer = -1
                DecodePseudoBankOffset(pseudo, expectBank, expectOffset)
                If bank <> expectBank OrElse offset <> expectOffset Then
                    problems.Add("pseudo " & AgcWord.ToOctal(pseudo) & " (" & a.ToString() &
                                 ") resolved to bank " & bank.ToString() & " offset " & offset.ToString() &
                                 ", expected bank " & expectBank.ToString() & " offset " & expectOffset.ToString())
                End If
            Next
            Return problems
        End Function

        ''' <summary>
        ''' The hardware bank and 2K offset a fixed pseudo address stands for.  This
        ''' is the definition the assembler and the rope image both must agree with:
        '''
        '''     00000..03777  the fixed-fixed pair's lower half, which the sources
        '''                   address as banks 00 and 01      -> bank 02
        '''     04000..07777  the pair's upper half (banks 02, 03) -> bank 02
        '''     010000..027777  ordinary banks                   -> 04..017
        '''     030000..077777  the super-bank module            -> 024..035
        ''' </summary>
        Public Shared Sub DecodePseudoBankOffset(pseudo As Integer, ByRef bank As Integer, ByRef offset As Integer)
            bank = -1
            offset = -1
            If pseudo < 0 OrElse pseudo > AgcWord.Mask15 Then Return
            If pseudo < 4096 Then
                ' The fixed-fixed pair's lower half, addressed as banks 00 and 01.
                bank = 2
                offset = pseudo
            ElseIf pseudo <= 8191 Then
                ' 04000..07777: the same pair, as banks 02 and 03.
                bank = 2
                offset = pseudo - 4096
            ElseIf pseudo <= 12287 Then
                ' 010000..027777: ordinary banks 04..017, each one 02000 block.
                bank = 4 + (pseudo - 4096) \ 2048
                offset = pseudo - (4096 + 2048 * (bank - 4))
            Else
                ' 030000..077777: the super-bank module, hardware banks 024..035.
                bank = 24 + (pseudo - 12288) \ 2048
                offset = pseudo - (12288 + 2048 * (bank - 24))
            End If
        End Sub

        ''' <summary>
        ''' The flat (pseudo) address of word 0 of a hardware bank, in the 2K offset
        ''' space the bank allocator works in.  A bank is 2K, so the block for bank n
        ''' is 02000 wide:
        '''
        '''     bank 02          006000   (the fixed-fixed pair; its low 1K, which the
        '''                               sources also address as banks 00 and 01, is
        '''                               what "006000 + offset" produces for offsets
        '''                               0..01777)
        '''     banks 04..023    010000 + 02000*(bank-04)
        '''     banks 024..035   030000 + 02000*(bank-024)
        ''' </summary>
        Public Shared Function BankPseudoBase(bank As Integer) As Integer
            If bank <= 3 Then Return 4096
            If bank >= 24 Then Return 12288 + 2048 * (bank - 24)
            Return 4096 + 2048 * (bank - 4)
        End Function

        ''' <summary>
        ''' The ".binsource" 1K block a fixed location belongs to.  The published
        ''' listings split the fixed-fixed pair into blocks 0..3 (its low 1K is
        ''' blocks 0 and 2, its high 1K blocks 1 and 3) and give the super-bank module
        ''' blocks 8..15; every other hardware bank is one 2K block.
        ''' </summary>
        Public Shared Function BinBankOf(hardwareBank As Integer, offset As Integer) As Integer
            Dim off As Integer = offset And 2047
            If hardwareBank = 2 Then Return If(off < 1024, 0, 1)
            If hardwareBank = 3 Then Return If(off < 1024, 0, 1)
            If hardwareBank >= 24 Then Return 8 + (hardwareBank - 24) * 2 + If(off < 1024, 0, 1)
            Return hardwareBank
        End Function

        ''' <summary>
        ''' The word-counter slot for a fixed location.  Counters are per 1K block,
        ''' and the fixed-fixed pair's low 1K is shared by listing blocks 0 and 2 (its
        ''' high 1K by 1 and 3), so each such pair shares one counter.
        ''' </summary>
        Public Shared Function BinSlotOf(hardwareBank As Integer, offset As Integer) As Integer
            If hardwareBank = 2 OrElse hardwareBank = 3 Then Return If((offset And 2047) < 1024, 0, 1)
            Return BinBankOf(hardwareBank, offset)
        End Function

        ''' <summary>
        ''' The owner of a counter slot, as the 2K offset of its first word inside the
        ''' hardware bank the slot belongs to.  This is the inverse of
        ''' <see cref="BinSlotOf"/> for the banks that share a pair.
        ''' </summary>
        Public Shared Sub BinSlotOwner(slot As Integer, ByRef hardwareBank As Integer, ByRef offset As Integer)
            If slot = 0 Then
                hardwareBank = 2
                offset = 0
                Return
            End If
            If slot = 1 Then
                hardwareBank = 2
                offset = 1024
                Return
            End If
            If slot >= 8 AndAlso slot <= 15 Then
                hardwareBank = 24 + (slot - 8) \ 2
                offset = ((slot - 8) Mod 2) * 1024
                Return
            End If
            hardwareBank = slot
            offset = 0
        End Sub

        ''' <summary>
        ''' The rope-image bank that holds a hardware bank.  The rope image indexes
        ''' fixed memory exactly the way the hardware indexes banks, so this is the
        ''' identity and a bank register value can be used as an array index.
        ''' </summary>
        Public Shared Function RopeBankOf(hardwareBank As Integer) As Integer
            Return hardwareBank
        End Function

        ''' <summary>
        ''' Resolve an assembler fixed address to the source bank it lives in and the
        ''' offset of the word inside that 2K bank.  SReg is "02000 + offset" with bit
        ''' 01000 marking the half of the bank, which is the same bit the hardware
        ''' reads out of an operand field.
        ''' </summary>
        Public Shared Sub ResolveFixed(address As AgcAddress, ByRef sourceBank As Integer, ByRef offset As Integer)
            sourceBank = -1
            offset = -1
            If Not address.IsFixed OrElse address.Invalid Then Return
            Dim sreg As Integer = address.SReg And AgcWord.Mask15
            If sreg < 2048 Then
                ' The low 1K of the fixed-fixed pair, addressed as 00000..03777 by
                ' the sources' banks 00 and 01.  Its hardware bank is 2.
                sourceBank = 2
                offset = sreg And 1023
                Return
            End If
            sourceBank = address.FB
            offset = (sreg - 2048) And &H7FF
        End Sub

        ''' <summary>
        ''' The ".binsource" 1K bank a fixed address belongs to.  The published
        ''' listings number the fixed-fixed pair as banks 0..3 and the super-bank
        ''' module as banks 8..15, and each remaining AGC bank as itself.
        ''' </summary>
        Public Shared Function BinBankFor(address As AgcAddress) As Integer
            Dim bank As Integer = -1
            Dim offset As Integer = -1
            ResolveFixed(address, bank, offset)
            If bank < 0 Then Return -1
            Return AgcAssembler.BinBankOf(bank, offset)
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
                Dim sourceBank As Integer = -1
                Dim offset As Integer = -1
                ResolveFixed(address, sourceBank, offset)
                If sourceBank >= 0 Then
                    ' The super-bank module (hardware banks 024..035) is indexed the
                    ' same way as every other bank; see RopeBankOf.
                    Image.SetFixed(RopeBankOf(sourceBank), offset, word)
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
            Dim sourceBank As Integer = -1
            Dim offset As Integer = -1
            ResolveFixed(address, sourceBank, offset)
            If sourceBank < 0 Then Return
            Dim slot As Integer = BinSlotOf(sourceBank, offset)
            If slot < 0 OrElse slot >= _usedInBank.Length Then Return
            Dim used As Integer = (offset And 1023) + count
            If used < 0 Then used = 0
            If used > 1024 Then used = 1024
            If used > _usedInBank(slot) Then _usedInBank(slot) = used
        End Sub

        ''' <summary>Number of words already assembled into a ".binsource" 1K bank.</summary>
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
