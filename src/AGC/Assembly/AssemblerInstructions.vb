' ============================================================================
'  AssemblerInstructions.vb -- operand encoders, pseudo-op handlers and the
'  basic-instruction encoder for the AGC assembler.
' ============================================================================

Imports System
Imports System.Collections.Generic
Imports System.Globalization

Namespace Agc

    Partial Public Class AgcAssembler

        ' ------------------------------------------------------------------
        ' Pseudo-op dispatch
        ' ------------------------------------------------------------------

        Private Sub ProcessPseudoOp(line As AgcLine)
            Dim op As String = line.Opcode
            Dim lineNo As Integer = line.LineNumber
            Dim file As String = line.File

            Select Case op

                Case "SETLOC", "LOC"
                    PseudoSetloc(line)

                Case "BANK"
                    PseudoBank(line)

                Case "BLOCK"
                    PseudoBlock(line)

                Case "EBANK="
                    PseudoEbank(line)

                Case "SBANK="
                    PseudoSbank(line)

                Case "EQUALS", "="
                    PseudoEquals(line, False)

                Case "=MINUS"
                    PseudoEqualsMinus(line)

                Case "=ECADR"
                    PseudoEqualsEcadr(line)

                Case "CHECK="
                    PseudoCheckEquals(line)

                Case "ERASE"
                    PseudoErase(line)

                Case "COUNT", "COUNT*", "BNKSUM", "MEMORY", "SUBRO", "SECSIZ"
                    ' Bookkeeping only.

                Case "OCT", "OCTAL"
                    EmitData(line, OpcodeDataFormat.Oct, 1)

                Case "2OCT"
                    EmitData(line, OpcodeDataFormat.Oct, 2)

                Case "DEC"
                    EmitData(line, OpcodeDataFormat.Dec, 1)

                Case "DEC*"
                    EmitData(line, OpcodeDataFormat.Dec, 1)

                Case "2DEC", "2DEC*"
                    EmitData(line, OpcodeDataFormat.Dec, 2)

                Case "MM"
                    EmitData(line, OpcodeDataFormat.Dec, 1)

                Case "VN", "NV"
                    EmitData(line, OpcodeDataFormat.VerbNoun, 1)

                Case "ADRES", "GENADR"
                    EmitAddress(line, AddressFormat.GenericSubAddress)

                Case "REMADR"
                    EmitAddress(line, AddressFormat.GenericSubAddress)

                Case "CADR", "FCADR"
                    EmitAddress(line, AddressFormat.Cadr)

                Case "2FCADR"
                    EmitAddress(line, AddressFormat.TwoFcadr)

                Case "ECADR"
                    EmitAddress(line, AddressFormat.Ecadr)

                Case "BBCON"
                    EmitAddress(line, AddressFormat.Bbcon)

                Case "BBCON*"
                    EmitAddress(line, AddressFormat.BbconStar)

                Case "2CADR", "2BCADR"
                    EmitAddress(line, AddressFormat.TwoCadr)

                Case "DNPTR"
                    EmitAddress(line, AddressFormat.DownlinkPointer)
                Case "-DNPTR"
                    EmitAddress(line, AddressFormat.DownlinkPointer)

                Case "1DNADR", "2DNADR", "3DNADR", "4DNADR", "5DNADR", "6DNADR",
                     "-1DNADR", "-2DNADR", "-3DNADR", "-4DNADR", "-5DNADR", "-6DNADR"
                    EmitDnaddr(line)

                Case "DNCHAN", "-DNCHAN"
                    EmitDnchan(line)

                Case "-GENADR"
                    EmitAddress(line, AddressFormat.GenericSubAddress)
                Case "-2CADR"
                    EmitAddress(line, AddressFormat.TwoCadr)

                Case Else
                    Diagnostics.Fatal(file, lineNo, "Unimplemented pseudo-op '" & op & "'.")
                    EmitAtCurrent(0)
                    Advance(1)
            End Select
        End Sub

        ' ------------------------------------------------------------------
        ' Location-counter pseudo-ops
        ' ------------------------------------------------------------------

        Private Sub PseudoSetloc(line As AgcLine)
            Dim operand As String = line.Operand
            Dim mod1 As String = line.Mod1

            If String.IsNullOrEmpty(operand) Then
                Diagnostics.Fatal(line.File, line.LineNumber, "SETLOC requires an operand.")
                Return
            End If

            If operand(0) = "+"c OrElse operand(0) = "-"c Then
                Dim v As Integer
                If AgcWord.TryParseInteger(operand, v) Then
                    _pc = AgcCodec.IncPc(_pc, v)
                End If
            Else
                Dim num As Integer
                If AgcWord.TryParseInteger(operand, num) AndAlso Not AgcSourceReader.IsRelativeLabel(operand) Then
                    _pc = AgcCodec.PseudoToAddress(num)
                    If _pc.Invalid Then
                        Diagnostics.Fatal(line.File, line.LineNumber, "SETLOC address out of range.")
                    End If
                Else
                    Dim name As String = operand
                    Dim off As Integer = 0
                    Dim numeric As Boolean = False
                    Dim a As AgcAddress = ResolveOperand(name, off, numeric)
                    If a.Invalid Then
                        Diagnostics.Fatal(line.File, line.LineNumber,
                                          "SETLOC symbol '" & operand & "' undefined.")
                    Else
                        _pc = a
                    End If
                End If
            End If

            Dim modValue As Integer
            If Not String.IsNullOrEmpty(mod1) AndAlso AgcWord.TryParseInteger(mod1, modValue) Then
                _pc = AgcCodec.IncPc(_pc, modValue)
            End If

            ' A SETLOC into banked erasable establishes the current E-bank.
            If _pc.IsErasable AndAlso _pc.Banked Then
                _ebank = _pc.EB
            End If
        End Sub

        Private Sub PseudoBank(line As AgcLine)
            Dim operand As String = line.Operand

            If String.IsNullOrEmpty(operand) Then
                ' Advance to the first unused word of the current bank.
                If Not _pc.IsFixed OrElse _pc.Invalid Then
                    Diagnostics.Fatal(line.File, line.LineNumber, "BANK without operand requires fixed memory.")
                    Return
                End If
                Dim sourceBank As Integer
                If _pc.SReg < 2048 Then
                    ' The low 1K of the fixed-fixed pair: bit 01000 says which of the
                    ' sources' banks 00 and 01 it is.
                    sourceBank = If((_pc.SReg And &H400) <> 0, 1, 0)
                ElseIf _pc.FB = 2 Then
                    sourceBank = 2
                Else
                    sourceBank = _pc.FB
                End If
                _pc = BankStartAddress(sourceBank, NextFreeInBank(sourceBank))
                Return
            End If

            Dim value As Integer
            If Not AgcWord.TryParseInteger(operand, value) Then
                Diagnostics.Fatal(line.File, line.LineNumber, "BANK pseudo-op has an invalid operand.")
                Return
            End If
            If value < 0 OrElse value > 35 Then          ' octal 043
                Diagnostics.Fatal(line.File, line.LineNumber, "BANK operand range is 00 to 43.")
                Return
            End If

            _pc = BankStartAddress(value, NextFreeInBank(value))
        End Sub

        ''' <summary>
        ''' The flat (pseudo) address of word <paramref name="offset"/> of a source
        ''' bank, where the offset runs 0..01777 over that bank's 2K.
        ''' </summary>
        Public Shared Function BankPseudoAddress(bank As Integer, offset As Integer) As Integer
            Return BankPseudoBase(bank) + (offset And 2047)
        End Function

        ''' <summary>
        ''' The first unused word of a source bank, in that bank's 2K offset space.
        ''' The fixed-fixed pair (banks 00..03) is addressed as bank 02; its low half
        ''' is the sources' banks 00 and 02 and its high half banks 01 and 03, so
        ''' both names for a half share one counter.
        ''' </summary>
        Private Function NextFreeInBank(bank As Integer) As Integer
            If bank <= 3 Then
                Dim low As Integer = BankUsage(0)
                If low < 1024 Then Return low
                Return 1024 + BankUsage(1)
            End If
            Dim slot As Integer = BinSlotOf(bank, 0)
            Dim used As Integer = BankUsage(slot)
            If used < 1024 Then Return used
            Dim highSlot As Integer = BinSlotOf(bank, 1024)
            If highSlot <> slot Then
                Dim high As Integer = BankUsage(highSlot)
                If high < 1024 Then Return 1024 + high
            End If
            Return used
        End Function

        ''' <summary>The segmented address of word <paramref name="offset"/> of a hardware bank.</summary>
        Private Shared Function BankStartAddress(bank As Integer, offset As Integer) As AgcAddress
            If offset < 0 Then offset = 0
            If offset > 2047 Then offset = 2047
            ' The fixed-fixed pair (hardware bank 02) is addressed as "02000 + offset"
            ' so that bit 01000 marks the half of the 2K bank, exactly as the
            ' hardware reads an operand field.  Every other bank is addressed as
            ' "02000 + offset" too, with FB naming the bank.
            Return AgcCodec.PseudoToAddress(BankPseudoBase(bank) + offset)
        End Function

        ''' <summary>
        ''' The first unused word of AGC bank <paramref name="bank"/>, in the 2K
        ''' offset space of that bank.  The word counters are per 1K block, following
        ''' the published listing's bank numbering, so the block the caller is
        ''' already in decides which counter to read.
        ''' </summary>
        Private Function NextFreeInBank(bank As Integer, offset As Integer) As Integer
            Dim half As Integer
            If bank <= 3 Then
                half = (bank Mod 2) * 1024
            Else
                half = If(offset < 1024, 0, 1024)
            End If
            Dim used As Integer = BankUsage(BinBankOf(bank, half))
            If used < 1024 Then Return half + used
            Dim nextHalf As Integer = Math.Min(half + 1024, 2047)
            Return nextHalf + BankUsage(BinBankOf(bank, nextHalf))
        End Function

        Private Sub PseudoBlock(line As AgcLine)
            Dim value As Integer
            If Not AgcWord.TryParseInteger(line.Operand, value) Then
                Diagnostics.Fatal(line.File, line.LineNumber, "BLOCK pseudo-op has no operand.")
                Return
            End If
            If value <> 0 AndAlso value <> 2 AndAlso value <> 3 Then
                Diagnostics.Fatal(line.File, line.LineNumber, "BLOCK operand must be 0, 2, or 3.")
                Return
            End If

            ' BLOCK 0   -> unswitched erasable
            ' BLOCK 2,3 -> the fixed-fixed area, 04000 and 06000 (the second halves
            '              of AGC banks 02 and 03).
            If value = 0 Then
                Dim a As New AgcAddress()
                a.Kind = AgcMemoryKind.Erasable
                a.Unbanked = True
                a.SReg = 0
                _ebank = 0
                _pc = a
            Else
                _pc = BankStartAddress(value, NextFreeInBank(value, 0))
            End If
        End Sub

        Private Sub PseudoEbank(line As AgcLine)
            If String.IsNullOrEmpty(line.Operand) Then Return
            Dim name As String = line.Operand
            Dim off As Integer = 0
            Dim numeric As Boolean = False
            Dim a As AgcAddress = ResolveOperand(name, off, numeric)
            If a.Invalid Then
                Diagnostics.Fatal(line.File, line.LineNumber,
                                  "EBANK= symbol '" & line.Operand & "' undefined.")
                Return
            End If
            If Not a.IsErasable Then
                Diagnostics.Fatal(line.File, line.LineNumber, "EBANK= destination not erasable.")
                Return
            End If
            If a.Banked Then
                _ebank = a.EB
            Else
                _ebank = a.SReg \ &H100
            End If
            _ebankOneShot = True
        End Sub

        Private Sub PseudoSbank(line As AgcLine)
            If String.IsNullOrEmpty(line.Operand) Then Return
            Dim name As String = line.Operand
            Dim off As Integer = 0
            Dim numeric As Boolean = False
            Dim a As AgcAddress = ResolveOperand(name, off, numeric)
            If a.Invalid Then
                Diagnostics.Fatal(line.File, line.LineNumber,
                                  "SBANK= symbol '" & line.Operand & "' undefined.")
                Return
            End If
            If Not a.IsFixed Then
                Diagnostics.Warn(line.File, line.LineNumber, "SBANK= destination not in fixed memory.")
                Return
            End If
            Dim bank As Integer = If(a.Banked, a.FB + 8 * If(a.Super, 1, 0), If(a.SReg < &H1000, 2, 3))
            If bank >= &H18 AndAlso bank <= &H1F Then
                _sbank = If(a.Super, 4, 3)
            End If
        End Sub

        ' ------------------------------------------------------------------
        ' Symbol definition pseudo-ops
        ' ------------------------------------------------------------------

        Private Sub PseudoEquals(line As AgcLine, isMinus As Boolean)
            If String.IsNullOrEmpty(line.Label) Then Return

            If String.IsNullOrEmpty(line.Operand) Then
                If isMinus Then
                    Diagnostics.Fatal(line.File, line.LineNumber, "Missing operand.")
                    Return
                End If
                Symbols.Define(line.Label, _pc, True, line.File, line.LineNumber)
                Return
            End If

            ' The sources separate the offset with whitespace, for example
            ' "FLAGWRD5 = STATE +5", which the reader splits into an operand and a
            ' modifier field.  They have to be recombined or the offset is lost and
            ' the equate silently becomes the un-offset address.
            Dim equateOperand As String = AgcSourceReader.CombineOperandAndOffset(line.Operand, line.Mod1, line.Mod2)
            Dim name As String = equateOperand
            Dim off As Integer = 0
            Dim numeric As Boolean = False
            Dim a As AgcAddress = ResolveOperand(name, off, numeric)
            If a.Invalid Then
                ' The operand names a symbol that has not been defined yet.  The AGC
                ' sources rely on this, for example "FLAGWRD5 = STATE +5" appears
                ' long before "STATE ERASE", so remember the equate and settle it
                ' once the whole file has been read.
                DeferEquate(line, equateOperand, isMinus, False)
                Return
            End If

            ' A pure numeric operand that names a memory location keeps both the
            ' address and the constant interpretation, matching yaYUL.
            If numeric Then
                a.IsConstant = True
            End If

            Symbols.Define(line.Label, a, True, line.File, line.LineNumber)
        End Sub

        Private Sub PseudoEqualsMinus(line As AgcLine)
            If String.IsNullOrEmpty(line.Label) Then Return
            If String.IsNullOrEmpty(line.Operand) Then
                Diagnostics.Fatal(line.File, line.LineNumber, "Missing operand.")
                Return
            End If
            Dim name As String = line.Operand
            Dim off As Integer = 0
            Dim numeric As Boolean = False
            Dim a As AgcAddress = ResolveOperand(name, off, numeric)
            If a.Invalid Then
                Diagnostics.Fatal(line.File, line.LineNumber, "Symbol '" & line.Operand & "' undefined.")
                Return
            End If
            Dim result As AgcAddress = AgcAddress.Constant(a.SReg - _pc.SReg)
            Symbols.Define(line.Label, result, True, line.File, line.LineNumber)
        End Sub

        Private Sub PseudoEqualsEcadr(line As AgcLine)
            If String.IsNullOrEmpty(line.Label) Then Return
            If String.IsNullOrEmpty(line.Operand) Then Return
            Dim name As String = line.Operand
            Dim off As Integer = 0
            Dim numeric As Boolean = False
            Dim a As AgcAddress = ResolveOperand(name, off, numeric)
            If a.Invalid Then
                Diagnostics.Fatal(line.File, line.LineNumber, "Symbol '" & line.Operand & "' undefined.")
                Return
            End If
            Symbols.Define(line.Label, AgcAddress.Constant(EcadrOf(a)), True, line.File, line.LineNumber)
        End Sub

        Private Sub PseudoCheckEquals(line As AgcLine)
            If String.IsNullOrEmpty(line.Label) Then
                Diagnostics.Fatal(line.File, line.LineNumber, "Label must be supplied for CHECK=.")
                Return
            End If
            Dim sym As AgcSymbol = Nothing
            If Not Symbols.TryGetValue(line.Label, sym) Then
                Diagnostics.Warn(line.File, line.LineNumber,
                                 "Symbol '" & line.Label & "' not yet defined, skipping CHECK=.")
                Return
            End If
            Dim name As String = line.Operand
            Dim off As Integer = 0
            Dim numeric As Boolean = False
            Dim a As AgcAddress = ResolveOperand(name, off, numeric)
            If a.Invalid Then
                Diagnostics.Warn(line.File, line.LineNumber,
                                 "CHECK= operand '" & line.Operand & "' undefined.")
                Return
            End If
            If sym.Address.PseudoValue <> a.PseudoValue Then
                Diagnostics.Fatal(line.File, line.LineNumber,
                                  "Address check failed for '" & line.Label & "'.")
            End If
        End Sub

        Private Sub PseudoErase(line As AgcLine)
            Dim value As Integer = 0
            If Not String.IsNullOrEmpty(line.Operand) Then
                If Not AgcWord.TryParseInteger(line.Operand, value) Then
                    Diagnostics.Fatal(line.File, line.LineNumber, "Illegal number in ERASE.")
                    Return
                End If
            End If
            If value < 0 Then
                Diagnostics.Warn(line.File, line.LineNumber, "ERASE increment is negative.")
                value = 0
            End If
            Advance(1 + value)
            If Not _pc.IsErasable Then
                Diagnostics.Fatal(line.File, line.LineNumber, "ERASE is not in erasable memory.")
            End If
        End Sub

        ' ------------------------------------------------------------------
        ' Data pseudo-ops
        ' ------------------------------------------------------------------

        Private Enum OpcodeDataFormat
            Oct
            Dec
            VerbNoun
        End Enum

        Private Sub EmitData(line As AgcLine, format As OpcodeDataFormat, words As Integer)
            Dim operand As String = line.Operand
            Dim mod1 As String = line.Mod1
            Dim mod2 As String = line.Mod2

            If String.IsNullOrEmpty(operand) Then
                Diagnostics.Fatal(line.File, line.LineNumber, "Operand is missing.")
                Dim zero(words - 1) As Integer
                EmitAndAdvance(zero)
                Return
            End If

            Select Case format

                Case OpcodeDataFormat.Oct
                    If words = 2 Then
                        EmitTwoOct(line)
                        Return
                    End If
                    Dim v As Integer
                    If Not ParseOctOperand(operand, v) Then
                        Diagnostics.Fatal(line.File, line.LineNumber, "Not an octal number: '" & operand & "'.")
                        EmitAndAdvance(New Integer() {0})
                        Return
                    End If
                    EmitAndAdvance(New Integer() {v And AgcWord.Mask15})

                Case OpcodeDataFormat.Dec
                    EmitDecimal(line, words)

                Case OpcodeDataFormat.VerbNoun
                    Dim ok As Boolean
                    Dim v As Integer = AgcWord.EncodeVerbNoun(operand & mod1, ok)
                    If Not ok Then
                        Diagnostics.Fatal(line.File, line.LineNumber, "Operand is not a decimal group.")
                    End If
                    EmitAndAdvance(New Integer() {v})
            End Select
        End Sub

        ''' <summary>Parse an OCT operand: optional sign, octal digits, optional "D" suffix.</summary>
        Private Shared Function ParseOctOperand(text As String, ByRef value As Integer) As Boolean
            Return AgcOctParser.TryParse(text, value)
        End Function

        Private Sub EmitTwoOct(line As AgcLine)
            Dim v1 As Integer, v2 As Integer
            Dim ok1 As Boolean = ParseOctOperand(line.Operand, v1)
            Dim ok2 As Boolean = ParseOctOperand(line.Mod1, v2)
            If Not ok1 Then
                Diagnostics.Fatal(line.File, line.LineNumber, "Not an octal number: '" & line.Operand & "'.")
                EmitAndAdvance(New Integer() {0, 0})
                Return
            End If
            If Not ok2 Then
                ' A single number is split across the two words.
                Dim combined As Long = CLng(v1 And AgcWord.Mask15)
                EmitAndAdvance(New Integer() {CInt((combined >> 15) And AgcWord.Mask15), CInt(combined And AgcWord.Mask15)})
                Return
            End If
            EmitAndAdvance(New Integer() {v1 And AgcWord.Mask15, v2 And AgcWord.Mask15})
        End Sub

        Private Sub EmitDecimal(line As AgcLine, words As Integer)
            Dim operand As String = line.Operand
            Dim mod1 As String = line.Mod1
            Dim mod2 As String = line.Mod2

            Dim numericText As String = operand
            Dim scale1 As String = mod1
            Dim scale2 As String = mod2
            Dim value As Double = 0.0
            Dim parseOk As Boolean = False

            ' Numbers written as "1.5 E-3 B+4" may be split across the fields.
            If Not String.IsNullOrEmpty(scale1) AndAlso (Char.ToUpperInvariant(scale1(0)) = "E"c OrElse Char.ToUpperInvariant(scale1(0)) = "B"c) Then
                ' scale factor alone in mod1
            ElseIf Not String.IsNullOrEmpty(operand) AndAlso (Char.ToUpperInvariant(operand(0)) = "E"c OrElse Char.ToUpperInvariant(operand(0)) = "B"c) Then
                ' only a scale factor was given
                numericText = "1"
                scale2 = scale1
                scale1 = operand
            End If

            Dim eIdx As Integer = IndexOfScale(numericText, "E"c)
            Dim bIdx As Integer = IndexOfScale(numericText, "B"c)
            If eIdx >= 0 OrElse bIdx >= 0 Then
                Dim first As Integer = If(eIdx < 0, bIdx, If(bIdx < 0, eIdx, Math.Min(eIdx, bIdx)))
                Dim second As Integer
                If eIdx >= 0 AndAlso bIdx >= 0 Then
                    second = Math.Max(eIdx, bIdx)
                Else
                    second = -1
                End If
                If second >= 0 Then
                    scale2 = numericText.Substring(second)
                    scale1 = numericText.Substring(first, second - first)
                    numericText = numericText.Substring(0, first)
                Else
                    scale1 = numericText.Substring(first)
                    numericText = numericText.Substring(0, first)
                End If
            ElseIf Not String.IsNullOrEmpty(scale1) AndAlso (scale1 = "E"c OrElse scale1 = "B"c) Then
                scale1 = scale1 & scale2
                scale2 = ""
            End If

            Dim valueText As String = numericText.Trim()
            If valueText.Length = 0 Then valueText = "0"
            Dim hasDecimal As Boolean = valueText.IndexOf("."c) >= 0

            Dim d As Double
            If Double.TryParse(valueText, NumberStyles.Float, CultureInfo.InvariantCulture, d) Then
                value = d
                parseOk = True
            ElseIf valueText.Length > 0 AndAlso (valueText(0) = "+"c OrElse valueText(0) = "-"c) AndAlso valueText.Length = 1 Then
                value = 1.0
                parseOk = True
            End If

            If Not parseOk Then
                Diagnostics.Fatal(line.File, line.LineNumber, "Not a decimal number: '" & operand & "'.")
                If words = 2 Then
                    EmitAndAdvance(New Integer() {0, 0})
                Else
                    EmitAndAdvance(New Integer() {0})
                End If
                Return
            End If

            If words = 2 Then
                Dim hi As Integer, lo As Integer
                Dim ok As Boolean
                AgcWord.EncodeDoublePrecision(value, scale1, scale2, hasDecimal, hi, lo, ok)
                If Not ok Then
                    Diagnostics.Warn(line.File, line.LineNumber,
                                     "Value '" & operand & "' out of range for 2DEC.")
                End If
                EmitAndAdvance(New Integer() {hi, lo})
            Else
                Dim ok As Boolean
                Dim v As Integer = AgcWord.EncodeSinglePrecision(value, scale1, scale2, hasDecimal, ok)
                If Not ok Then
                    Diagnostics.Warn(line.File, line.LineNumber,
                                     "Value '" & operand & "' out of range for DEC.")
                End If
                EmitAndAdvance(New Integer() {v})
            End If
        End Sub

        ''' <summary>Find an 'E' or 'B' scale marker that is not part of the exponent digits.</summary>
        Private Shared Function IndexOfScale(text As String, marker As Char) As Integer
            For i As Integer = 1 To text.Length - 1
                If Char.ToUpperInvariant(text(i)) = marker Then
                    ' Must be followed by an optional sign and digits.
                    Dim j As Integer = i + 1
                    If j < text.Length AndAlso (text(j) = "+"c OrElse text(j) = "-"c) Then j += 1
                    If j < text.Length AndAlso Char.IsDigit(text(j)) Then Return i
                End If
            Next
            Return -1
        End Function

        ''' <summary>ECADR value of an erasable address (bank * 0400 + offset).</summary>
        Private Shared Function EcadrOf(a As AgcAddress) As Integer
            If a.Unbanked Then Return a.SReg And AgcWord.Mask15
            Return ((a.EB And 7) * &H100) Or ((a.SReg - &H1400) And &HFF)
        End Function

    End Class

End Namespace
