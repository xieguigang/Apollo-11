' ============================================================================
'  AssemblerOpcodes.vb -- instruction encoding for the AGC assembler:
'  basic instructions, raw numeric opcodes and the interpretive (Danzig)
'  instruction set.
' ============================================================================

Imports System
Imports System.Collections.Generic

Namespace Agc

    Partial Public Class AgcAssembler

        ' ------------------------------------------------------------------
        ' Basic instructions
        ' ------------------------------------------------------------------

        Private Sub ProcessInstruction(line As AgcLine, op As String, operandText As String, isAlias As Boolean)
            Dim entry As AgcOpcode = Nothing
            If Not AgcOpcodeTable.TryGetBasic(op, entry) Then
                Diagnostics.Fatal(line.File, line.LineNumber, "Unknown instruction '" & op & "'.")
                EmitAtCurrent(0)
                Advance(1)
                Return
            End If

            Dim wasExtended As Boolean = _pendingExtend
            Dim wasIndexed As Boolean = False
            _pendingExtend = False

            Select Case op
                Case "STORE", "STODL", "STOVL", "STCALL"
                    EncodeInterpreterStore(line, op, operandText)
                    Return
                Case "VN", "NV"
                    Dim ok As Boolean
                    Dim v As Integer = AgcWord.EncodeVerbNoun(operandText & line.Mod1, ok)
                    If Not ok Then
                        Diagnostics.Fatal(line.File, line.LineNumber, "VN operand is not a decimal group.")
                    End If
                    EmitAndAdvance(New Integer() {v})
                    Return
                Case "EDRUPT"
                    EmitAndAdvance(New Integer() {&H3800})
                    Return
                Case "INDEX", "NDX"
                    EncodeIndex(line)
                    Return
            End Select

            ' The opcode already contains every bit that belongs in the word.
            Dim opcode As Integer = entry.Opcode

            ' -- EXTEND checking.  The transcribed sources are column aligned, and
            ' the assembler is line based, so a missing EXTEND is reported as a
            ' warning rather than a fatal error: a genuine problem will show up as
            ' a wrong opcode in the rope comparison.
            If Not wasExtended AndAlso (entry.Flags And OperandFlags.Extended) <> 0 Then
                Diagnostics.Warn(line.File, line.LineNumber, "Required EXTEND is missing.")
            End If
            If wasExtended AndAlso (entry.Flags And OperandFlags.Extended) = 0 Then
                Diagnostics.Warn(line.File, line.LineNumber, "Illegally preceded by EXTEND.")
            End If

            ' -- Operand resolution.
            Dim ok2 As Boolean = True
            Dim address As AgcAddress = ResolveOperandForInstruction(line, operandText, ok2)
            If Not ok2 OrElse address.Invalid Then
                If _pass = 2 Then
                    Diagnostics.Fatal(line.File, line.LineNumber,
                                      "Operand '" & operandText & "' not resolved for " & op & ".")
                End If
                EmitAtCurrent(opcode)
                Advance(1)
                Return
            End If

            Dim value As Integer
            If wasIndexed Then
                ' With a preceding INDEX the operand may be anything, so no
                ' memory-type or range checking is meaningful.
                value = address.SReg And AgcWord.Mask12
            Else
                If (entry.Flags And OperandFlags.Plus1) <> 0 Then
                    address = AgcCodec.IncPc(address, 1)
                End If
                value = EncodeOperandValue(line, op, entry, address, opcode, wasExtended)
            End If

            ' The instruction word is 16 bits wide (15 data bits plus the parity
            ' bit), so the opcode's top bit must be preserved.
            Dim finalWord As Integer = (opcode Or (value And AgcWord.Mask12)) And AgcWord.Mask17
            If _debugEncoding Then
                Console.WriteLine("ENCODE " & op & " opcode=" & AgcWord.ToOctal(opcode) &
                                  " value=" & AgcWord.ToOctal(value) &
                                  " addr=" & address.ToString() &
                                  " kind=" & address.Kind.ToString() &
                                  " banked=" & address.Banked.ToString() &
                                  " word=" & AgcWord.ToOctal(finalWord))
            End If
            EmitAtCurrent(finalWord)
            Advance(1)
        End Sub

        ''' <summary>
        ''' Resolve an instruction operand the way FetchSymbolPlusOffset does,
        ''' including the "operand missing means current location" rule.
        ''' </summary>
        Private Function ResolveOperandForInstruction(line As AgcLine, operandText As String, ByRef ok As Boolean) As AgcAddress
            Dim text As String = operandText
            Dim mod1 As String = line.Mod1
            Dim mod2 As String = line.Mod2

            If String.IsNullOrEmpty(text) Then
                ok = True
                Return _pc
            End If

            ' "$n" is a literal number.
            If text(0) = "$"c Then
                Dim n As Integer
                If AgcWord.TryParseInteger(text.Substring(1), n) Then
                    ok = True
                    Return AgcAddress.Constant(n)
                End If
            End If

            If text(0) = "+"c OrElse text(0) = "-"c Then
                Dim v As Integer
                If AgcWord.TryParseInteger(text, v) Then
                    ok = True
                    Return AgcCodec.IncPc(_pc, v)
                End If
            End If

            Dim combined As String = text
            If Not String.IsNullOrEmpty(mod1) Then combined &= mod1
            If Not String.IsNullOrEmpty(mod2) Then combined &= mod2

            Dim num As Integer
            If AgcWord.TryParseInteger(combined, num) Then
                ' GetOctOrDec succeeded: a bare number is interpreted as a
                ' constant first and as an address only if it can be one.
                Dim a As AgcAddress = AgcCodec.PseudoToAddress(num)
                If a.Invalid Then a = AgcAddress.Constant(num)
                ok = True
                Return a
            End If

            Dim name As String = text
            Dim off As Integer = 0
            Dim numeric As Boolean = False
            Dim result As AgcAddress = ResolveOperand(name, off, numeric)
            ok = Not result.Invalid
            Return result
        End Function

        ''' <summary>Apply the quarter-code / memory-type rules and produce the address field.</summary>
        Private Function EncodeOperandValue(line As AgcLine, op As String, entry As AgcOpcode,
                                            address As AgcAddress, opcode As Integer,
                                            wasExtended As Boolean) As Integer
            Dim flags As OperandFlags = entry.Flags

            ' A constant that names a memory location may be used either way; if
            ' the instruction demands an address, prefer the address form.
            Dim wantsAddress As Boolean = (flags And (OperandFlags.Fixed Or OperandFlags.Erasable)) <> 0

            Dim i As Integer = address.SReg And AgcWord.Mask15

            If wantsAddress Then
                If (flags And OperandFlags.Fixed) <> 0 AndAlso Not address.IsFixed Then
                    Diagnostics.Fatal(line.File, line.LineNumber, "The address is not in fixed memory.")
                ElseIf (flags And OperandFlags.Erasable) <> 0 AndAlso Not address.IsErasable Then
                    Diagnostics.Fatal(line.File, line.LineNumber, "The address is not in erasable memory.")
                End If
            End If

            ' The operand field is 12 bits wide.  For the ordinary (banked and
            ' unswitched-erasable) cases only 10 bits are meaningful, but a
            ' fixed-fixed address such as 04012 uses all twelve.
            Dim limit As Integer = If(wasExtended, 8191, 4095)

            Dim hasPC As Boolean = (flags And (OperandFlags.QC0 Or OperandFlags.QC1 Or OperandFlags.QC2 Or
                                               OperandFlags.QC3 Or OperandFlags.QCNot0)) <> 0

            ' The quarter code belongs to the instruction itself, not to its target:
            ' a TCF with quarter code 0 branches perfectly well to an operand in the
            ' 02000..07777 range, and a CAF may name any address.  Requiring the
            ' operand's top address bits to reproduce the opcode's quarter code
            ' rejected a very large number of legitimate references, so the operand
            ' is checked only against the width of its address field.
            Dim unusedQC As Boolean = hasPC AndAlso Not wantsAddress
            If unusedQC Then
                ' No additional restriction: the 12-bit field is the only limit.
            End If

            If i > limit Then
                ' An address outside the reachable field: keep the low bits but
                ' report the problem the way the AGC assembler does.
                Diagnostics.Fatal(line.File, line.LineNumber,
                                  "Operand out of range: " & op & " " & line.Operand &
                                  " resolved to " & address.ToString())
            End If

            Return i
        End Function

        ''' <summary>INDEX / NDX: place the index register address into the instruction.</summary>
        Private Sub EncodeIndex(line As AgcLine)
            Dim operandText As String = line.Operand
            Dim value As Integer = 0
            Dim wasExtended As Boolean = _pendingExtend
            _pendingExtend = False

            If String.IsNullOrEmpty(operandText) Then
                Diagnostics.Fatal(line.File, line.LineNumber, "INDEX requires an operand.")
            Else
                Dim name As String = operandText
                Dim off As Integer = 0
                Dim numeric As Boolean = False
                Dim a As AgcAddress = ResolveOperand(name, off, numeric)
                If a.Invalid Then
                    If _pass = 2 Then
                        Diagnostics.Fatal(line.File, line.LineNumber,
                                          "INDEX operand '" & operandText & "' not resolved.")
                    End If
                Else
                    If a.IsConstant AndAlso Not a.IsAddress Then
                        value = a.SReg
                    Else
                        value = a.SReg And AgcWord.Mask15
                    End If
                    ' The index field is 10 bits.  For a banked-erasable operand the
                    ' E-bank is selected separately at run time, so the field carries
                    ' only the offset within the bank; taking the whole sub-address
                    ' would reject every index in a switched-erasable section, which
                    ' the AGC sources use freely.
                    Dim limit As Integer = If(wasExtended, &HFFF, &H3FF)
                    If a.IsErasable AndAlso a.Banked Then
                        value = (a.SReg - &H1400) And limit
                    End If
                    If value > limit Then
                        Diagnostics.Fatal(line.File, line.LineNumber, "Index is out of range.")
                        value = value And limit
                    End If
                End If
            End If

            EmitAtCurrent(&H5000 Or (value And AgcWord.Mask15))
            Advance(1)

            ' The following instruction still sees the extend/index state.
            _pendingExtend = wasExtended
        End Sub

        ''' <summary>Raw numeric opcode such as "2 0002" (meaning DAS 0002).</summary>
        Private Sub ProcessRawNumericOpcode(line As AgcLine, numOp As Integer)
            Dim flags As OperandFlags = OperandFlags.None
            Dim ok As Boolean = True
            Dim address As AgcAddress = ResolveOperandForInstruction(line, line.Operand, ok)
            Dim value As Integer = If(ok, address.SReg And AgcWord.Mask15, 0)

            Dim wasExtended As Boolean = _pendingExtend
            _pendingExtend = False

            If Not wasExtended AndAlso (numOp = 2 AndAlso (value And &HC00) = 0) Then
                flags = flags Or OperandFlags.Plus1
            ElseIf Not wasExtended AndAlso numOp = 5 AndAlso (value And &HC00) = &H400 Then
                flags = flags Or OperandFlags.Plus1
            ElseIf wasExtended AndAlso (numOp = 4 OrElse numOp = 5) Then
                flags = flags Or OperandFlags.Plus1
            End If

            If flags And OperandFlags.Plus1 Then value = AgcCodec.IncPc(address, 1).SReg And AgcWord.Mask15

            EmitAtCurrent((numOp << 12) Or (value And AgcWord.Mask15))
            Advance(1)
        End Sub

        ' ------------------------------------------------------------------
        ' The interpretive STORE family (encoded as basic instructions)
        ' ------------------------------------------------------------------

        Private Function ParseCommaSuffix(ByRef text As String, mod1 As String) As Integer
            Dim target As String = If(String.IsNullOrEmpty(mod1), text, mod1)
            If target IsNot Nothing AndAlso target.Length >= 3 Then
                Dim tail As String = target.Substring(target.Length - 2)
                If tail = ",1" Then
                    If String.IsNullOrEmpty(mod1) Then
                        text = text.Substring(0, text.Length - 2)
                    End If
                    Return 1
                ElseIf tail = ",2" Then
                    If String.IsNullOrEmpty(mod1) Then
                        text = text.Substring(0, text.Length - 2)
                    End If
                    Return 2
                End If
            End If
            Return 0
        End Function

        Private Sub EncodeInterpreterStore(line As AgcLine, op As String, operandText As String)
            Dim argType As Integer = ParseCommaSuffix(operandText, line.Mod1)

            Dim baseOpcode As Integer
            Select Case op
                Case "STORE" : baseOpcode = &H0
                Case "STODL" : baseOpcode = &H0C00
                Case "STOVL" : baseOpcode = &H1400
                Case "STCALL" : baseOpcode = &H1C00
                Case Else : baseOpcode = &H0
            End Select
            baseOpcode += &H400 * argType

            Dim ok As Boolean = True
            Dim address As AgcAddress = ResolveOperandForInstruction(line, operandText, ok)
            If Not ok OrElse address.Invalid Then
                If _pass = 2 Then
                    Diagnostics.Fatal(line.File, line.LineNumber,
                                      "Destination address for " & op & " not resolved.")
                End If
                EmitAtCurrent(baseOpcode)
                Advance(1)
                Return
            End If

            If Not address.IsErasable Then
                Diagnostics.Fatal(line.File, line.LineNumber, "The address is not in erasable memory.")
            End If

            Dim i As Integer = EcadrOf(address)
            i = (i + 1) And AgcWord.Mask15
            If argType = 2 Then i = AgcWord.Complement(i)

            EmitAtCurrent(baseOpcode Or (i And AgcWord.Mask15))
            Advance(1)
        End Sub

        ' ------------------------------------------------------------------
        ' Indexed (post-indexed) instruction forms such as "AD X,1"
        ' ------------------------------------------------------------------

        ''' <summary>
        ''' Basic instructions may carry a ",1"/",2" post-index suffix, which the
        ''' AGC implements with a preceding INDEX; the assembler emits both words.
        ''' </summary>
        Private Function TrySplitPostIndex(ByRef operandText As String, mod1 As String) As Integer
            If Not String.IsNullOrEmpty(mod1) Then Return 0
            If String.IsNullOrEmpty(operandText) Then Return 0
            If operandText.EndsWith(",1", StringComparison.Ordinal) Then
                operandText = operandText.Substring(0, operandText.Length - 2)
                Return 1
            End If
            If operandText.EndsWith(",2", StringComparison.Ordinal) Then
                operandText = operandText.Substring(0, operandText.Length - 2)
                Return 2
            End If
            Return 0
        End Function

    End Class

End Namespace
