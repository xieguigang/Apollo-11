' ============================================================================
'  AssemblerInterpreter.vb -- encoding of the AGC "interpretive" (Danzig)
'  instruction set.
'
'  Every interpretive instruction is a one or two word entry:
'
'      word 1 : (077777 & ~((code1+1) | ((code2+1) << 7)))
'      word 2+ : operand addresses, one word per operand
'
'  When a source line carries two interpretive opcodes (for example "DLOAD" on
'  one line immediately followed by an operand-less instruction) the second
'  opcode is packed into bits 7-13 of the same word.
' ============================================================================

Imports System
Imports System.Collections.Generic

Namespace Agc

    Partial Public Class AgcAssembler

        ''' <summary>Assemble an interpretive opcode and start its operand queue.</summary>
        Private Sub ProcessInterpretiveOpcode(line As AgcLine, entry As AgcOpcode)
            _pendingExtend = False

            Dim code1 As Integer = entry.InterpreterCode
            Dim code2 As Integer = -1

            ' A second interpretive opcode may appear in the operand field, e.g.
            ' "DLOAD" with "SETPD" following on the same line.
            Dim operandText As String = line.Operand
            Dim secondEntry As AgcOpcode = Nothing
            If Not String.IsNullOrEmpty(operandText) Then
                Dim candidate As String = operandText
                If AgcOpcodeTable.TryGetInterpretive(candidate, secondEntry) Then
                    code2 = secondEntry.InterpreterCode
                    operandText = ""
                Else
                    secondEntry = Nothing
                End If
            End If

            Dim word As Integer = &H7F And (code1 + 1)
            If code2 >= 0 Then
                word = word Or (&HFE00 And ((code2 + 1) << 7))
            End If
            word = (AgcWord.Mask15 And Not word) And AgcWord.Mask15
            EmitAndAdvance(New Integer() {word})

            ' Queue up the operands.  An interpretive instruction's first operand
            ' is written on the same source line as the opcode ("DLOAD A"), so it
            ' must be consumed here; only operands that do not fit on this line
            ' are left pending for the following lines.
            _interpNnnn.Clear()
            _interpSwitchInc.Clear()
            _interpSwitchInvert.Clear()
            _interpOperandsPending = 0
            _interpIndex = 0

            QueueInterpreterOperands(entry)
            If secondEntry IsNot Nothing Then QueueInterpreterOperands(secondEntry)

            If _interpOperandsPending > 0 AndAlso Not String.IsNullOrEmpty(line.Operand) Then
                Dim synthetic As New AgcLine()
                synthetic.File = line.File
                synthetic.LineNumber = line.LineNumber
                synthetic.Opcode = ""
                synthetic.Operand = line.Operand
                synthetic.Mod1 = line.Mod1
                synthetic.Mod2 = line.Mod2
                ProcessInterpretiveOperandLine(synthetic)
            End If
        End Sub

        Private Sub QueueInterpreterOperands(entry As AgcOpcode)
            If entry.InterpreterOperands >= 1 Then
                _interpSwitchInc.Add(entry.ArgType0)
                _interpSwitchInvert.Add(0)
                _interpNnnn.Add(entry.InterpreterNnnn)
                _interpOperandsPending += 1
            End If
            If entry.InterpreterOperands >= 2 Then
                _interpSwitchInc.Add(entry.ArgType1)
                _interpSwitchInvert.Add(0)
                _interpNnnn.Add(0)
                _interpOperandsPending += 1
            End If
        End Sub

        ' ------------------------------------------------------------------
        ' Operand words
        ' ------------------------------------------------------------------

        Private Sub ProcessInterpretiveOperandLine(line As AgcLine)
            ' Each interpretive operand may consume one source line; the operand
            ' text may sit in the Opcode or the operand field.
            Dim text As String = line.Operand
            Dim mod1 As String = line.Mod1

            If String.IsNullOrEmpty(text) AndAlso Not String.IsNullOrEmpty(line.Opcode) Then
                ' Data word in the Opcode column, e.g. "2DEC .5" following a
                ' VLOAD; treat the Opcode as the operand.
                text = line.Opcode
                mod1 = line.Operand
            End If

            If String.IsNullOrEmpty(text) Then
                ' An empty operand word is legal (it becomes 0).
                EmitAtCurrent(0)
                Advance(1)
                _interpOperandsPending -= 1
                PopInterpreterOperandState()
                Return
            End If

            Dim argType As Integer = 0
            If mod1 IsNot Nothing AndAlso mod1.Length >= 3 Then
                If mod1.EndsWith(",1", StringComparison.Ordinal) Then
                    argType = 1
                    mod1 = mod1.Substring(0, mod1.Length - 2)
                ElseIf mod1.EndsWith(",2", StringComparison.Ordinal) Then
                    argType = 2
                    mod1 = mod1.Substring(0, mod1.Length - 2)
                End If
            End If
            If text.Length >= 3 Then
                If text.EndsWith(",1", StringComparison.Ordinal) Then
                    argType = 1
                    text = text.Substring(0, text.Length - 2)
                ElseIf text.EndsWith(",2", StringComparison.Ordinal) Then
                    argType = 2
                    text = text.Substring(0, text.Length - 2)
                End If
            End If

            Dim index As Integer = _interpNnnn.Count - _interpOperandsPending
            Dim nnnn As Integer = 0
            Dim switchInc As Integer = 0
            If index >= 0 AndAlso index < _interpNnnn.Count Then
                nnnn = _interpNnnn(index)
                switchInc = _interpSwitchInc(index)
            End If

            ' Resolve the operand.
            Dim resolved As AgcAddress
            Dim isConstant As Boolean = False
            Dim constantValue As Integer = 0

            Dim name As String = text
            Dim off As Integer = 0

            If text(0) = "+"c OrElse text(0) = "-"c Then
                Dim v As Integer
                If AgcWord.TryParseInteger(text, v) Then
                    resolved = AgcCodec.IncPc(_pc, v)
                Else
                    Dim dummy As Boolean = False
                    resolved = ResolveOperand(name, off, dummy)
                End If
            Else
                Dim numeric As Boolean = False
                resolved = ResolveOperand(name, off, numeric)
                If numeric AndAlso (resolved.IsConstant OrElse Not resolved.IsAddress) Then
                    isConstant = True
                    constantValue = resolved.SReg
                End If
            End If

            If resolved.Invalid Then
                If _pass = 2 Then
                    Diagnostics.Fatal(line.File, line.LineNumber,
                                      "Interpretive operand '" & text & "' not resolved.")
                End If
                EmitAtCurrent(0)
                Advance(1)
            Else
                Dim encoded As Integer
                If isConstant Then
                    encoded = EncodeInterpreterConstant(constantValue, nnnn, switchInc, argType)
                Else
                    encoded = EncodeInterpreterAddress(resolved, switchInc, argType)
                End If
                If _debugEncoding Then
                    Console.WriteLine("IOPND op=" & _pass.ToString() & " pc=" & _pc.ToString() &
                                      " text=[" & text & "] resolved=" & resolved.ToString() &
                                      " fixed=" & resolved.IsFixed.ToString() &
                                      " banked=" & resolved.Banked.ToString() &
                                      " const=" & isConstant.ToString() &
                                      " constVal=" & AgcWord.ToOctal(constantValue) &
                                      " switchInc=" & switchInc.ToString() &
                                      " enc=" & AgcWord.ToOctal(encoded))
                End If
                EmitAtCurrent(encoded)
                Advance(1)
            End If

            _interpOperandsPending -= 1
            PopInterpreterOperandState()
        End Sub

        Private Sub PopInterpreterOperandState()
            If _interpOperandsPending <= 0 Then
                _interpOperandsPending = 0
                _interpNnnn.Clear()
                _interpSwitchInc.Clear()
                _interpSwitchInvert.Clear()
            End If
        End Sub

        ''' <summary>
        ''' Encode a numeric interpretive operand.  This mirrors the constant
        ''' branch of yaYUL's ParseInterpretiveOperand().
        ''' </summary>
        Private Shared Function EncodeInterpreterConstant(value As Integer, nnnn As Integer,
                                                          switchKind As Integer, argType As Integer) As Integer
            Dim word As Integer
            Dim kind As Integer = nnnn And 3

            If kind = 1 Then
                ' Switch instruction: 00WWWWWWNNNNBBBB with value = 15*W + B.
                Dim v As Integer = value And AgcWord.Mask15
                word = (((v \ 15) And &H3F) << 8) Or (v Mod 15)
                word = word Or (&H1E0 And nnnn)
            ElseIf kind = 2 Then
                ' Shift instruction: the shift count is the value.
                word = value And AgcWord.Mask15
                word = word Or (nnnn And AgcWord.Mask15 And Not 3)
            Else
                Dim i As Integer = value
                If i < 0 Then i -= 1
                i = i And AgcWord.Mask15
                If (i And &H4000) <> 0 Then
                    word = i
                Else
                    ' A constant below 040000 is re-interpreted as a pseudo address.
                    Dim a As AgcAddress = AgcCodec.PseudoToAddress(i)
                    If a.Invalid OrElse Not a.IsAddress Then
                        word = i
                    Else
                        Return EncodeInterpreterAddress(a, switchKind, argType)
                    End If
                End If
            End If

            If switchKind <> 0 Then
                word = (word + 1) And AgcWord.Mask15
                word = word And &H3FFF
                If argType = 2 Then word = AgcWord.Mask15 And Not word
            End If
            Return word And AgcWord.Mask15
        End Function

        ''' <summary>
        ''' Encode a resolved address as an interpretive operand.  Erasable
        ''' addresses are 10 bits (E-bank in bits 8-10), fixed addresses use the
        ''' 15-bit banked form.
        ''' </summary>
        Private Shared Function EncodeInterpreterAddress(a As AgcAddress, switchKind As Integer,
                                                         argType As Integer) As Integer
            Dim sreg As Integer = a.SReg And AgcWord.Mask15
            Dim word As Integer

            If a.IsErasable Then
                If a.Unbanked Then
                    word = sreg
                Else
                    word = ((a.EB And 7) * &H400) Or ((sreg - &H1400) And &H3FF)
                End If
            ElseIf a.IsFixed Then
                If Not a.Banked Then
                    word = sreg
                Else
                    word = ((a.FB And &H1F) * &H2000) Or ((sreg - &H800) And &H1FFF)
                End If
            Else
                word = sreg
            End If

            If switchKind <> 0 Then
                word = (word + 1) And AgcWord.Mask15
                word = word And &H3FFF
                If argType = 2 Then word = AgcWord.Mask15 And Not word
            End If
            Return word And AgcWord.Mask15
        End Function

    End Class

End Namespace
