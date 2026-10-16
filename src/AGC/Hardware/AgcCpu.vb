' ============================================================================
'  AgcCpu.vb -- the Block II AGC central processor.
'
'  The instruction cycle is modelled on the yaAGC reference engine, which is
'  authoritative for the Block II hardware:
'
'    * Z holds the 12-bit instruction address; a pending INDEX value is added
'      to the whole fetched instruction word (with end-around carry) before it
'      is decoded.
'    * The 6-bit extended opcode is (word >> 9), with bit 6 set when the
'      preceding instruction was EXTEND (TC 6).
'    * The operand address is the low 12 bits of the instruction; the memory
'      region and bank are derived from those bits and the FBANK/BBANK/EBANK
'      registers, exactly as in the real machine.
'
'  Timing: one Step() executes one instruction, charging the machine cycles the
'  real hardware needs.  The machine-cycle counter drives the 1/3200 second
'  scaler which drives TIME1..TIME6 and the interrupt structure.
' ============================================================================

Imports System
Imports System.Collections.Generic

Namespace Agc

    Public Class AgcCpu

        ' ------------------------------------------------------------------
        ' Erasable register addresses
        ' ------------------------------------------------------------------

        Public Const RegA As Integer = 0
        Public Const RegL As Integer = 1
        Public Const RegQ As Integer = 2
        ''' <summary>FBANK (the register the CPU uses for fixed bank selection).</summary>
        Public Const RegFBANK As Integer = 4
        Public Const RegZ As Integer = 5
        ''' <summary>BBANK (fixed bank selection for interrupt vectors).</summary>
        Public Const RegBBANK As Integer = 6
        Public Const RegZERO As Integer = 7
        Public Const RegARUPT As Integer = 8
        Public Const RegLRUPT As Integer = 9
        Public Const RegQRUPT As Integer = 10
        Public Const RegZRUPT As Integer = 13
        Public Const RegBBRUPT As Integer = 14
        Public Const RegBRUPT As Integer = 15
        Public Const RegCYR As Integer = 16
        Public Const RegSR As Integer = 17
        Public Const RegCYL As Integer = 18
        Public Const RegEDOP As Integer = 19
        Public Const RegTIME2 As Integer = 20
        Public Const RegTIME1 As Integer = 21
        Public Const RegTIME3 As Integer = 22
        Public Const RegTIME4 As Integer = 23
        Public Const RegTIME5 As Integer = 24
        Public Const RegTIME6 As Integer = 25
        Public Const RegCDUX As Integer = 26
        Public Const RegCDUY As Integer = 27
        Public Const RegCDUZ As Integer = 28
        Public Const RegOPTY As Integer = 29
        Public Const RegOPTX As Integer = 30
        Public Const RegPIPAX As Integer = 31
        Public Const RegPIPAY As Integer = 32
        Public Const RegPIPAZ As Integer = 33
        Public Const RegBMAGX As Integer = 34
        Public Const RegBMAGY As Integer = 35
        Public Const RegBMAGZ As Integer = 36
        Public Const RegINLINK As Integer = 37
        Public Const RegRNRAD As Integer = 38
        Public Const RegGYROCTR As Integer = 39
        Public Const RegCDUXCMD As Integer = 40
        Public Const RegCDUYCMD As Integer = 41
        Public Const RegCDUZCMD As Integer = 42
        Public Const RegCDUTCMD As Integer = 43
        Public Const RegCDUSCMD As Integer = 44
        Public Const RegEMSD As Integer = 45
        Public Const RegLEMONM As Integer = 46
        Public Const RegLOCALARM As Integer = 47
        Public Const RegBANKALRM As Integer = 48

        ''' <summary>Addresses below this value live in the processor registers.</summary>
        Private Const REG16 As Integer = 3

        ' ------------------------------------------------------------------
        ' State
        ' ------------------------------------------------------------------

        Public ReadOnly Memory As AgcMemory
        Public ReadOnly Io As AgcIo

        Public A As Integer
        Public A16 As Integer
        Public L As Integer
        Public Q As Integer
        ''' <summary>Program counter (Z).</summary>
        Public Z As Integer
        ''' <summary>Value written back to Z at the end of the instruction.</summary>
        Public NextZ As Integer
        Public IndexValue As Integer
        Public ExtraCode As Boolean
        Public SubstituteInstruction As Boolean
        Public PendFlag As Boolean
        Public PendDelay As Integer
        Public ExtraDelay As Integer

        Public AllowInterrupt As Boolean
        Public InIsr As Boolean
        ''' <summary>Pending interrupt requests indexed by priority (1 = highest).</summary>
        Public ReadOnly InterruptRequests(10) As Boolean

        Public Property InstructionCount As Long
        Public Property CycleCount As Long

        Public Property Halted As Boolean
        Public Property HaltReason As String = ""

        ''' <summary>Optional per-instruction trace hook.</summary>
        Public Property Tracer As Action(Of AgcCpu)

        ''' <summary>Alarm codes raised by the software (via the ALARM routine).</summary>
        Public ReadOnly Alarms As New List(Of Integer)()

        ''' <summary>Last instruction executed (for tracing and diagnostics).</summary>
        Public Property LastInstruction As Integer
        Public Property LastOpcode As Integer
        Public Property LastAddress12 As Integer

        Private _keepExtraCode As Boolean
        Private _tookBzf As Boolean
        Private _tookBzmf As Boolean

        ''' <summary>True once the program has run off into an illegal state.</summary>
        Public Property CpuRestartCount As Integer

        Public Sub New(memory As AgcMemory, io As AgcIo)
            Me.Memory = memory
            Me.Io = io
        End Sub

        ' ------------------------------------------------------------------
        ' Reset
        ' ------------------------------------------------------------------

        Public Sub Reset()
            A = 0 : A16 = 0 : L = 0 : Q = 0
            ' The AGC's fresh start enters the rope at address 04000, which is
            ' bank 02 of module 2 with the bank registers cleared.
            Z = &H800
            NextZ = Z
            IndexValue = 0
            ExtraCode = False
            SubstituteInstruction = False
            PendFlag = False
            PendDelay = 0
            ExtraDelay = 0
            AllowInterrupt = False
            InIsr = False
            For i As Integer = 0 To InterruptRequests.Length - 1
                InterruptRequests(i) = False
            Next
            InstructionCount = 0
            CycleCount = 0
            Halted = False
            HaltReason = ""
            CpuRestartCount = 0
            Memory.EBank = 0
            Array.Clear(Memory.Fbank, 0, Memory.Fbank.Length)
            Io.Reset()
        End Sub

        ''' <summary>Force a restart (the equivalent of the AGC's GOJAM).</summary>
        Public Sub RequestRestart()
            Q = Z And &H3FFF
            Z = &H800
            NextZ = Z
            InIsr = False
            AllowInterrupt = True
            ExtraCode = False
            IndexValue = 0
            For i As Integer = 1 To 10
                InterruptRequests(i) = False
            Next
            CpuRestartCount += 1
            Io.OnRestart()
        End Sub

        ' ------------------------------------------------------------------
        ' Erasable access with register semantics
        ' ------------------------------------------------------------------

        Public Function GetErasable(address As Integer) As Integer
            Dim a As Integer = address And AgcWord.Mask12
            Select Case a
                Case RegA : Return A And AgcWord.Mask15
                Case RegL : Return L And AgcWord.Mask15
                Case RegQ : Return Q And AgcWord.Mask15
                Case RegZERO : Return 0
            End Select
            Return Memory.ReadErasable(a)
        End Function

        Public Sub SetErasable(address As Integer, value As Integer)
            Dim a As Integer = address And AgcWord.Mask12
            value = value And AgcWord.Mask15
            Select Case a
                Case RegA
                    A = value : Return
                Case RegL
                    L = value : Return
                Case RegQ
                    Q = value : Return
                Case RegZERO
                    Memory.WriteErasable(RegZERO, 0) : Return
                Case RegZ
                    NextZ = value
                    Memory.WriteErasable(RegZ, value) : Return
                Case RegCYR
                    If (value And 1) <> 0 Then value = (value >> 1) Or &H4000 Else value = value >> 1
                    Memory.WriteErasable(RegCYR, value And AgcWord.Mask15) : Return
                Case RegSR
                    If (value And &H4000) <> 0 Then value = (value >> 1) Or &H4000 Else value = value >> 1
                    Memory.WriteErasable(RegSR, value And AgcWord.Mask15) : Return
                Case RegCYL
                    If (value And &H4000) <> 0 Then
                        value = ((value << 1) + 1) And AgcWord.Mask15
                    Else
                        value = (value << 1) And AgcWord.Mask15
                    End If
                    Memory.WriteErasable(RegCYL, value) : Return
                Case RegEDOP
                    Memory.WriteErasable(RegEDOP, (value >> 7) And &H7F) : Return
            End Select
            Memory.WriteErasable(a, value)
        End Sub

        ''' <summary>Read a word using a 12-bit address field.</summary>
        Private Function ReadAddress12(address12 As Integer) As Integer
            Dim a As Integer = address12 And AgcWord.Mask12
            If a < REG16 Then Return GetErasable(a)
            If a < &H400 Then Return Memory.ReadErasable(a)
            Return SignExtend(Memory.ReadFixed(a))
        End Function

        Private Sub WriteAddress12(address12 As Integer, value As Integer)
            Dim a As Integer = address12 And AgcWord.Mask12
            If a < &H400 Then
                SetErasable(a, value)
                Return
            End If
            Memory.WriteFixed(a, OverflowCorrected(value))
        End Sub

        Private Function ReadAddress10(address10 As Integer) As Integer
            Dim a As Integer = address10 And AgcWord.Mask10
            If a < REG16 Then Return GetErasable(a)
            If a < &H400 Then Return Memory.ReadErasable(a)
            Return SignExtend(Memory.ReadFixed(a))
        End Function

        Private Sub WriteAddress10(address10 As Integer, value As Integer)
            Dim a As Integer = address10 And AgcWord.Mask10
            If a < &H400 Then
                SetErasable(a, value)
                Return
            End If
            Memory.WriteFixed(a, OverflowCorrected(value))
        End Sub

        ' ------------------------------------------------------------------
        ' 16-bit helpers
        ' ------------------------------------------------------------------

        Public Shared Function SignExtend(word As Integer) As Integer
            word = word And AgcWord.Mask15
            If (word And &H4000) <> 0 Then Return word Or &H8000
            Return word
        End Function

        Public Shared Function AddSP16(x As Integer, y As Integer) As Integer
            Dim sum As Integer = (x And &HFFFF) + (y And &HFFFF)
            If (sum And &H10000) <> 0 Then sum = (sum And &HFFFF) + 1
            Return sum And &HFFFF
        End Function

        Public Shared Function OverflowCorrected(value As Integer) As Integer
            Dim v As Integer = value And &HFFFF
            If (v And &H8000) <> 0 Then Return (v + 1) And AgcWord.Mask15
            Return v And AgcWord.Mask15
        End Function

        ''' <summary>
        ''' The overflow indication of an addition, in AGC format: 0 (none),
        ''' 040000 (+1) or 077777 (-1 / "minus zero").
        ''' </summary>
        Public Shared Function ValueOverflowed(value As Integer) As Integer
            Dim v As Integer = value And &HFFFF
            If (v And &H8000) <> 0 Then Return &H4000
            Return v And &H4000
        End Function

        Public Shared Function NegateSP(word As Integer) As Integer
            Return AgcWord.Negate(word)
        End Function

        Public Shared Function AbsSP(value As Integer) As Integer
            Dim v As Integer = value And &HFFFF
            If (v And &H8000) <> 0 Then Return (-(v And &H7FFF)) And &H7FFF
            Return v And &H7FFF
        End Function

        Public Shared Function AgcToNative(word As Integer) As Integer
            word = word And AgcWord.Mask15
            If (word And &H4000) <> 0 Then Return -(word And &H3FFF)
            Return word
        End Function

        Public Shared Function NativeToAgc(value As Integer) As Integer
            If value < 0 Then Return (AgcWord.Mask15 + value) And AgcWord.Mask15
            Return value And AgcWord.Mask15
        End Function

        Private Shared Function Agc2Cpu(value As Integer) As Integer
            value = value And &HFFFF
            If (value And &H8000) <> 0 Then Return -(value And &H7FFF)
            Return value And &H7FFF
        End Function

        Private Shared Function Cpu2Agc(value As Integer) As Integer
            If value < 0 Then Return (AgcWord.Mask15 And Not (-value)) And AgcWord.Mask15
            Return value And AgcWord.Mask15
        End Function

        ''' <summary>Diminished absolute value, as computed by CCS.</summary>
        Private Shared Function DAbs(value As Integer) As Integer
            Dim v As Integer = value And &HFFFF
            If (v And &H8000) <> 0 Then v = &H7FFF And Not v
            If v > 1 Then Return v - 1
            Return 0
        End Function

        Private Shared Function DAbs16(value As Integer) As Integer
            Dim v As Integer = value And &HFFFF
            If (v And &H8000) <> 0 Then v = &HFFFF And Not v
            If v > 1 Then Return v - 1
            Return 0
        End Function

        ' ------------------------------------------------------------------
        ' Machine cycles
        ' ------------------------------------------------------------------

        Private Shared ReadOnly InstructionTiming As Integer() = {
            0, 0, 0, 0, 1, 0, 0, 0,
            2, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 2, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1}

        Private Shared ReadOnly ExtracodeTiming As Integer() = {
            1, 1, 1, 1, 5, 0, 0, 0,
            1, 1, 1, 1, 2, 2, 2, 2,
            2, 2, 2, 2, 1, 1, 1, 1,
            1, 0, 0, 0, 2, 2, 2, 2}

        Public Sub StepOne()
            If Halted Then Return

            If ExtraDelay > 0 Then
                ExtraDelay -= 1
                CycleCount += 1
                Return
            End If
            If PendFlag AndAlso PendDelay > 0 Then
                PendDelay -= 1
                CycleCount += 1
                Return
            End If

            ExecuteInstruction()
            InstructionCount += 1
        End Sub

        ' ------------------------------------------------------------------
        ' The instruction cycle
        ' ------------------------------------------------------------------

        Private Sub ExecuteInstruction()
            Dim programCounter As Integer = Z And AgcWord.Mask12

            Dim instruction As Integer
            If SubstituteInstruction Then
                instruction = GetErasable(RegBRUPT)
            Else
                Dim word As Integer
                If programCounter < &H400 Then
                    word = GetErasable(programCounter)
                Else
                    word = Memory.ReadFixed(programCounter)
                End If
                ' An index value is applied to the instruction word itself.  The
                ' word is 16 bits wide (including the parity bit), and its top
                ' three bits are the opcode, so 17 working bits must be kept.
                Dim sum As Integer = (IndexValue And AgcWord.Mask17) + (word And AgcWord.Mask17)
                If sum > AgcWord.Mask17 Then
                    ' Overflow off the top wraps in the AGC's adder.
                    sum = sum And AgcWord.Mask15
                End If
                instruction = sum And AgcWord.Mask17
            End If

            Dim extraCode As Boolean = ExtraCode
            Dim extendedOpcode As Integer = (instruction >> 9) And &H3F
            If extraCode Then extendedOpcode = extendedOpcode Or &H40

            Dim quarterCode As Integer = instruction And &H3800
            Dim address12 As Integer = instruction And AgcWord.Mask12
            Dim address10 As Integer = instruction And AgcWord.Mask10
            Dim address9 As Integer = instruction And AgcWord.Mask9

            LastInstruction = instruction
            LastOpcode = extendedOpcode
            LastAddress12 = address12
            If Tracer IsNot Nothing Then Tracer.Invoke(Me)

            Dim accumulator16 As Integer = (A16 << 15) Or (A And AgcWord.Mask15)
            Dim overflow As Integer = ValueOverflowed(accumulator16)

            ' --------------------------------------------------------------
            ' Interrupts
            ' --------------------------------------------------------------
            Dim interruptRequested As Boolean = False
            Dim requestNumber As Integer = 0

            If (AllowInterrupt AndAlso Not InIsr AndAlso Not ExtraCode AndAlso Not PendFlag _
                AndAlso overflow = 0 AndAlso instruction <> 3 AndAlso instruction <> 4 AndAlso instruction <> 6) _
               OrElse extendedOpcode = &H47 Then

                For i As Integer = 1 To 10
                    If InterruptRequests(i) Then
                        InterruptRequests(i) = False
                        requestNumber = i
                        NextZ = &H800 + 4 * i
                        interruptRequested = True
                        Exit For
                    End If
                Next

                If Not interruptRequested AndAlso extendedOpcode = &H47 Then
                    ' EDRUPT with no pending request vectors to address 0.
                    NextZ = 0
                    requestNumber = 0
                    interruptRequested = True
                End If
            End If

            If interruptRequested Then
                SetErasable(RegZRUPT, programCounter + 1)
                SetErasable(RegBRUPT, instruction)
                ExtraCode = False
                IndexValue = 0
                SubstituteInstruction = False
                InIsr = True
                ExtraDelay += 1
                CycleCount += 2
                FinishInstruction(False, True)
                Return
            End If

            ' --------------------------------------------------------------
            ' Multi-cycle instructions
            ' --------------------------------------------------------------
            If Not PendFlag Then
                Dim idx As Integer = (quarterCode >> 10) And 7
                Dim mct As Integer
                If extraCode Then
                    mct = If(idx < ExtracodeTiming.Length, ExtracodeTiming(idx), 1)
                Else
                    mct = If(idx < InstructionTiming.Length, InstructionTiming(idx), 0)
                End If
                If mct > 0 Then
                    PendFlag = True
                    PendDelay = mct - 1
                    CycleCount += 1
                    Return
                End If
            Else
                PendFlag = False
            End If

            IndexValue = 0
            SubstituteInstruction = False

            NextZ = (1 + Z) And &HFFFF
            Z = NextZ

            Dim executedTc As Boolean = False
            _tookBzf = False
            _tookBzmf = False

            Select Case extendedOpcode

                ' ----------------------------------------------------------
                ' TC (000-007)
                ' ----------------------------------------------------------
                Case &H0 To &H7
                    Dim valueK As Integer = address12
                    If valueK = 3 Then
                        AllowInterrupt = True
                    ElseIf valueK = 4 Then
                        AllowInterrupt = False
                    ElseIf valueK = 6 Then
                        ExtraCode = True
                        _keepExtraCode = True
                    Else
                        If valueK <> RegQ Then Q = NextZ And &H3FFF
                        NextZ = address12
                        executedTc = True
                    End If

                ' ----------------------------------------------------------
                ' CCS (010-011)
                ' ----------------------------------------------------------
                Case &H8, &H9
                    ' CCS (010-011)
                    Dim operand16 As Integer
                    If address10 < REG16 Then
                        Dim valueK As Integer = (A16 << 15) Or (A And AgcWord.Mask15)
                        operand16 = ValueOverflowed(valueK)
                        A = DAbs16(valueK) : A16 = 0
                    Else
                        operand16 = ReadAddress10(address10) And AgcWord.Mask15
                        A = DAbs(operand16) : A16 = 0
                    End If

                    If operand16 = 0 Then
                        ' +0: fall through
                    ElseIf operand16 = AgcWord.Mask15 Then
                        NextZ += 3
                    ElseIf (operand16 And &H4000) <> 0 Then
                        NextZ += 2
                    Else
                        NextZ += 1
                    End If

                ' ----------------------------------------------------------
                ' TCF and the 012-017 class
                ' ----------------------------------------------------------
                Case &HA To &HF
                    ' TCF and the 012-017 class
                    NextZ = address12
                    executedTc = True

                ' ----------------------------------------------------------
                ' DAS / DDOUBL (020-021)
                ' ----------------------------------------------------------
                Case &H10, &H11
                    ' DAS / DDOUBL (020-021)
                    ' DAS / DDOUBL (020-021)
                    Dim msw As Integer, lsw As Integer
                    If address10 = RegL Then
                        lsw = AddSP16(L, L)
                        msw = AddSP16(accumulator16, accumulator16)
                        If (&H6000 And lsw) = &H2000 Then
                            msw = AddSP16(msw, 1)
                        ElseIf (&H6000 And lsw) = &H4000 Then
                            msw = AddSP16(msw, SignExtend(AgcWord.Mask15))
                        End If
                        lsw = OverflowCorrected(lsw)
                        A = OverflowCorrected(msw) : A16 = 0
                        L = SignExtend(lsw) And AgcWord.Mask15
                    Else
                        Dim wordAddress As Integer = address10 And AgcWord.Mask10
                        Dim lswIn As Integer = ReadAddress10(address10)
                        Dim mswIn As Integer = ReadAddress10(address10 - 1)
                        lsw = AddSP16(L, SignExtend(lswIn))
                        msw = AddSP16(accumulator16, SignExtend(mswIn))
                        If (&H6000 And lsw) = &H2000 Then
                            msw = AddSP16(msw, 1)
                        ElseIf (&H6000 And lsw) = &H4000 Then
                            msw = AddSP16(msw, SignExtend(AgcWord.Mask15))
                        End If
                        lsw = OverflowCorrected(lsw)
                        If (&H6000 And msw) = &H4000 Then
                            A = AgcWord.Mask15
                        ElseIf (&H6000 And msw) = &H2000 Then
                            A = 1
                        Else
                            A = 0
                        End If
                        A16 = 0
                        L = 0
                        WriteAddress10(address10, SignExtend(lsw))
                        WriteAddress10(address10 - 1, OverflowCorrected(msw))
                    End If

                ' ----------------------------------------------------------
                ' LXCH / ZL (022-023)
                ' ----------------------------------------------------------
                Case &H12, &H13
                    ' LXCH / ZL (022-023)
                    If address10 = RegL Then
                        ' no-op
                    ElseIf address10 = RegZERO Then
                        L = 0
                    ElseIf address10 < &H400 Then
                        Dim operand16 As Integer = L
                        L = GetErasable(address10)
                        SetErasable(address10, operand16)
                        If address10 = RegZ Then NextZ = GetErasable(RegZ)
                    Else
                        Dim address As Integer = address10 And AgcWord.Mask10
                        Dim operand16 As Integer = SignExtend(Memory.ReadFixed(address))
                        Memory.WriteFixed(address, OverflowCorrected(L))
                        L = OverflowCorrected(operand16) And AgcWord.Mask15
                    End If

                ' ----------------------------------------------------------
                ' INCR (024-025)
                ' ----------------------------------------------------------
                Case &H14, &H15
                    ' INCR (024-025)
                    If address10 < &H400 Then
                        SetErasable(address10, AddSP16(1, GetErasable(address10)))
                    Else
                        Dim address As Integer = address10 And AgcWord.Mask10
                        Dim sum As Integer = AddSP16(1, SignExtend(Memory.ReadFixed(address)))
                        Memory.WriteFixed(address, OverflowCorrected(sum))
                        TriggerCounterInterrupt(address10, sum)
                    End If

                ' ----------------------------------------------------------
                ' ADS (026-027)
                ' ----------------------------------------------------------
                Case &H16, &H17
                    ' ADS (026-027)
                    Dim sum As Integer
                    If address10 = RegA Then
                        sum = AddSP16(accumulator16, accumulator16)
                        A = OverflowCorrected(sum)
                        A16 = If((sum And &H8000) <> 0, 1, 0)
                    Else
                        sum = AddSP16(accumulator16, SignExtend(ReadAddress10(address10)))
                        A = OverflowCorrected(sum)
                        A16 = If((sum And &H8000) <> 0, 1, 0)
                        WriteAddress10(address10, OverflowCorrected(sum))
                    End If

                ' ----------------------------------------------------------
                ' CA (030-037)
                ' ----------------------------------------------------------
                Case &H18 To &H1F
                    If address12 <> RegA Then
                        A = ReadAddress12(address12) And AgcWord.Mask15
                        A16 = 0
                    End If

                ' ----------------------------------------------------------
                ' CS (040-047)
                ' ----------------------------------------------------------
                Case &H20 To &H27
                    ' CS (040-047)
                    If address12 = RegA Then
                        A = AgcWord.Complement(A)
                    Else
                        A = AgcWord.Complement(ReadAddress12(address12)) And AgcWord.Mask15
                        A16 = 0
                    End If

                ' ----------------------------------------------------------
                ' INDEX (050-051) and RESUME (0150-0157)
                ' ----------------------------------------------------------
                Case &H28, &H29
                    ' INDEX (050-051)
                    If address10 = &HF Then
                        ResumeFromInterrupt()
                    Else
                        IndexValue = OverflowCorrected(ReadAddress10(address10))
                    End If

                Case &H68 To &H6F
                    ' RESUME (0150-0157)
                    If address12 = &HE Then
                        ResumeFromInterrupt()
                    Else
                        IndexValue = OverflowCorrected(ReadAddress12(address12))
                        _keepExtraCode = True
                    End If

                ' ----------------------------------------------------------
                ' DXCH (052-053)
                ' ----------------------------------------------------------
                Case &H2A, &H2B
                    ' DXCH (052-053)
                    If address10 = RegL Then
                        L = SignExtend(OverflowCorrected(L)) And AgcWord.Mask15
                    Else
                        Dim operand16 As Integer = ReadAddress10(address10)
                        WriteAddress10(address10, OverflowCorrected(L))
                        L = OverflowCorrected(operand16) And AgcWord.Mask15
                        If address10 = RegZ Then NextZ = GetErasable(RegZ)
                        operand16 = ReadAddress10(address10 - 1)
                        WriteAddress10(address10 - 1, OverflowCorrected(accumulator16))
                        A = OverflowCorrected(operand16) : A16 = 0
                        If address10 = RegZ + 1 Then NextZ = GetErasable(RegZ)
                    End If

                ' ----------------------------------------------------------
                ' TS / TCAA / OVSK (054-055)
                ' ----------------------------------------------------------
                Case &H2C, &H2D
                    ' TS / TCAA / OVSK (054-055)
                    If address10 = RegA Then
                        If overflow <> 0 Then NextZ += 1
                    ElseIf address10 = RegZ Then
                        NextZ = accumulator16 And AgcWord.Mask15
                        If overflow <> 0 Then
                            A = OverflowCorrected(accumulator16) : A16 = 0
                        End If
                    Else
                        WriteAddress10(address10, OverflowCorrected(accumulator16))
                        If overflow <> 0 Then
                            A = OverflowCorrected(accumulator16) : A16 = 0
                            NextZ += 1
                        End If
                    End If

                ' ----------------------------------------------------------
                ' XCH (056-057)
                ' ----------------------------------------------------------
                Case &H2E, &H2F
                    ' XCH (056-057)
                    If address10 <> RegA Then
                        Dim accumulator As Integer = OverflowCorrected(accumulator16)
                        Dim operand16 As Integer = ReadAddress10(address10)
                        A = SignExtend(operand16) And AgcWord.Mask15
                        A16 = 0
                        WriteAddress10(address10, accumulator)
                        If address10 = RegZ Then NextZ = GetErasable(RegZ)
                    End If

                ' ----------------------------------------------------------
                ' AD (060-067)
                ' ----------------------------------------------------------
                Case &H30 To &H37
                    ' AD (060-067)
                    Dim accumulator As Integer
                    If address12 = RegA Then
                        accumulator = AddSP16(accumulator16, accumulator16)
                    Else
                        accumulator = AddSP16(accumulator16, SignExtend(ReadAddress12(address12)))
                    End If
                    A = OverflowCorrected(accumulator)
                    A16 = If((accumulator And &H8000) <> 0, 1, 0)

                ' ----------------------------------------------------------
                ' MASK (070-077)
                ' ----------------------------------------------------------
                Case &H38 To &H3F
                    ' MASK (070-077)
                    A = OverflowCorrected(accumulator16) And ReadAddress12(address12)
                    A16 = 0

                ' ----------------------------------------------------------
                ' READ / WRITE / RAND / WAND / ROR / WOR / RXOR / EDRUPT
                ' ----------------------------------------------------------
                Case &H40
                    A = ReadIoWord(address9) And AgcWord.Mask15
                    A16 = 0
                Case &H41
                    WriteIoWord(address9, OverflowCorrected(accumulator16))
                Case &H42
                    A = OverflowCorrected(accumulator16) And ReadIoWord(address9)
                    A16 = 0
                Case &H43
                    Dim operand16 As Integer = OverflowCorrected(accumulator16) And ReadIoWord(address9)
                    WriteIoWord(address9, operand16)
                    A = SignExtend(operand16) And AgcWord.Mask15
                    A16 = 0
                Case &H44
                    A = OverflowCorrected(accumulator16) Or ReadIoWord(address9)
                    A16 = 0
                Case &H45
                    Dim operand16 As Integer = OverflowCorrected(accumulator16) Or ReadIoWord(address9)
                    WriteIoWord(address9, operand16)
                    A = SignExtend(operand16) And AgcWord.Mask15
                    A16 = 0
                Case &H46
                    A = OverflowCorrected(accumulator16) Xor ReadIoWord(address9)
                    A16 = 0
                Case &H47
                    ' EDRUPT handled by the interrupt logic.

                ' ----------------------------------------------------------
                ' DV (0110-0111)
                ' ----------------------------------------------------------
                Case &H48, &H49
                    ExecuteDivide(address10, accumulator16)

                ' ----------------------------------------------------------
                ' BZF (0112-0117)
                ' ----------------------------------------------------------
                Case &H4A To &H4F
                    If OverflowCorrected(accumulator16) = 0 Then
                        NextZ = address12
                        executedTc = True
                        _tookBzf = True
                    End If

                ' ----------------------------------------------------------
                ' MSU (0120-0121)
                ' ----------------------------------------------------------
                Case &H50, &H51
                    Dim ui As Integer, uj As Integer
                    ui = OverflowCorrected(accumulator16)
                    uj = AgcWord.Mask15 And Not ReadAddress10(address10)
                    Dim diff As Integer = (ui And &HFFFF) + (uj And &HFFFF) + 1
                    If (diff And &H4000) <> 0 Then
                        diff = diff Or &H8000
                        diff -= 1
                    End If
                    If address10 = RegQ Then
                        A = diff And AgcWord.Mask15
                    Else
                        A = SignExtend(diff) And AgcWord.Mask15
                    End If
                    A16 = 0

                ' ----------------------------------------------------------
                ' QXCH / ZQ (0122-0123)
                ' ----------------------------------------------------------
                Case &H52, &H53
                    If address10 = RegQ Then
                        ' no-op
                    ElseIf address10 = RegZERO Then
                        Q = 0
                    ElseIf address10 < &H400 Then
                        Dim operand16 As Integer = Q
                        Q = GetErasable(address10)
                        SetErasable(address10, operand16)
                        If address10 = RegZ Then NextZ = GetErasable(RegZ)
                    Else
                        Dim address As Integer = address10 And AgcWord.Mask10
                        Dim operand16 As Integer = OverflowCorrected(Q)
                        Q = SignExtend(Memory.ReadFixed(address)) And AgcWord.Mask15
                        Memory.WriteFixed(address, operand16)
                    End If

                ' ----------------------------------------------------------
                ' AUG (0124-0125)
                ' ----------------------------------------------------------
                Case &H54, &H55
                    Dim operand16 As Integer = ReadAddress10(address10)
                    Dim increment As Integer = If((operand16 And &H4000) = 0, 1, AgcWord.Mask15)
                    Dim sum As Integer = AddSP16(increment, SignExtend(operand16))
                    WriteAddress10(address10, OverflowCorrected(sum))
                    TriggerCounterInterrupt(address10, sum)

                ' ----------------------------------------------------------
                ' DIM (0126-0127)
                ' ----------------------------------------------------------
                Case &H56, &H57
                    Dim operand16 As Integer = ReadAddress10(address10)
                    If operand16 <> 0 AndAlso operand16 <> AgcWord.Mask15 Then
                        Dim increment As Integer = If((operand16 And &H4000) = 0, AgcWord.Mask15, 1)
                        Dim sum As Integer = AddSP16(increment, SignExtend(operand16))
                        WriteAddress10(address10, OverflowCorrected(sum))
                    End If

                ' ----------------------------------------------------------
                ' DCA (0130-0137) / DCS (0140-0147)
                ' ----------------------------------------------------------
                Case &H58 To &H5F
                    If address12 = RegL Then
                        L = SignExtend(OverflowCorrected(L)) And AgcWord.Mask15
                    Else
                        L = OverflowCorrected(ReadAddress12(address12)) And AgcWord.Mask15
                        A = ReadAddress12(address12 - 1) And AgcWord.Mask15
                        A16 = 0
                    End If

                Case &H60 To &H67
                    If address12 = RegL Then
                        A = AgcWord.Complement(A)
                        L = AgcWord.Complement(L)
                        L = SignExtend(OverflowCorrected(L)) And AgcWord.Mask15
                    Else
                        L = AgcWord.Complement(ReadAddress12(address12)) And AgcWord.Mask15
                        A = AgcWord.Complement(ReadAddress12(address12 - 1)) And AgcWord.Mask15
                        A16 = 0
                    End If

                ' ----------------------------------------------------------
                ' SU (0160-0161)
                ' ----------------------------------------------------------
                Case &H70, &H71
                    Dim accumulator As Integer
                    If address10 = RegA Then
                        accumulator = AgcWord.Mask15
                    Else
                        accumulator = AddSP16(accumulator16, SignExtend(AgcWord.Negate(ReadAddress10(address10))))
                    End If
                    A = OverflowCorrected(accumulator)
                    A16 = If((accumulator And &H8000) <> 0, 1, 0)

                ' ----------------------------------------------------------
                ' BZMF (0162-0167)
                ' ----------------------------------------------------------
                Case &H72 To &H77
                    Dim accumulator As Integer = OverflowCorrected(accumulator16)
                    If accumulator = 0 OrElse (accumulator And &H4000) <> 0 Then
                        NextZ = address12
                        executedTc = True
                        _tookBzmf = True
                    End If

                ' ----------------------------------------------------------
                ' MP (0170-0177)
                ' ----------------------------------------------------------
                Case &H78 To &H7F
                    ExecuteMultiply(address12, accumulator16)

                Case Else
                    Halted = True
                    HaltReason = "Unrecognised opcode " & Convert.ToString(extendedOpcode, 8)
            End Select

            Dim cycles As Integer = 1
            Dim qcIndex As Integer = (quarterCode >> 10) And 7
            If extraCode Then
                cycles += If(qcIndex < ExtracodeTiming.Length, ExtracodeTiming(qcIndex), 1)
            Else
                cycles += If(qcIndex < InstructionTiming.Length, InstructionTiming(qcIndex), 0)
            End If
            CycleCount += cycles

            FinishInstruction(executedTc, interruptRequested)
        End Sub

        Private Sub FinishInstruction(executedTc As Boolean, wasInterrupt As Boolean)
            Z = NextZ And &HFFFF
            SetErasable(RegZERO, 0)
            Io.OnCycleEnd(CycleCount)

            If Not _keepExtraCode Then
                If Not wasInterrupt Then ExtraCode = False
            End If
            _keepExtraCode = False

            If Not SubstituteInstruction Then Z = Z And AgcWord.Mask12

            ' Refresh the bank registers used for fixed memory decoding.  The
            ' CPU mirrors FBANK (address 4) and BBANK (address 6) into the
            ' hardware bank registers.
            Dim regFBANK As Integer = GetErasable(RegFBANK) And AgcWord.Mask15
            Dim regBBANK As Integer = GetErasable(RegBBANK) And AgcWord.Mask15
            Dim regEBANK As Integer = Memory.ReadErasable(3) And AgcWord.Mask15
            Memory.EBank = (regEBANK >> 8) And 7
            Memory.Fbank(0) = (regFBANK >> 10) And &H37
            Memory.Fbank(1) = (regBBANK >> 10) And &H37

            L = SignExtend(OverflowCorrected(L)) And AgcWord.Mask15

            Io.UpdateBankIO(regEBANK, regFBANK, regBBANK, Memory.EBank)
        End Sub

        Private Sub ResumeFromInterrupt()
            NextZ = GetErasable(RegZRUPT) - 1
            InIsr = False
            SubstituteInstruction = True
        End Sub

        Private Function ReadIoWord(address9 As Integer) As Integer
            Dim a As Integer = address9 And &H1FF
            If a = RegL OrElse a = RegQ Then Return GetErasable(a)
            Return Io.ReadChannel(a) And AgcWord.Mask15
        End Function

        Private Sub WriteIoWord(address9 As Integer, value As Integer)
            Dim a As Integer = address9 And &H1FF
            If a = RegL OrElse a = RegQ Then
                SetErasable(a, value)
                Return
            End If
            Io.WriteChannel(a, value And AgcWord.Mask15)
        End Sub

        Private Sub TriggerCounterInterrupt(address10 As Integer, sum As Integer)
            If ValueOverflowed(sum) = 0 Then Return
            Select Case address10 And AgcWord.Mask10
                Case RegTIME1
                    Io.CounterPINC(RegTIME2)
                Case RegTIME5
                    InterruptRequests(2) = True
                Case RegTIME3
                    InterruptRequests(3) = True
                Case RegTIME4
                    InterruptRequests(4) = True
            End Select
        End Sub

        ' ------------------------------------------------------------------
        ' MP
        ' ------------------------------------------------------------------

        Private Sub ExecuteMultiply(address12 As Integer, accumulator16 As Integer)
            Dim operand16 As Integer = OverflowCorrected(accumulator16)
            Dim other As Integer = ReadAddress12(address12)

            Dim msWord As Integer, lsWord As Integer
            If other = 0 OrElse other = AgcWord.Mask15 Then
                msWord = 0 : lsWord = 0
            ElseIf operand16 = 0 OrElse operand16 = AgcWord.Mask15 Then
                If (operand16 = 0 AndAlso (other And &H2000) <> 0) OrElse
                   (operand16 = AgcWord.Mask15 AndAlso (other And &H2000) = 0) Then
                    msWord = AgcWord.Mask15 : lsWord = AgcWord.Mask15
                Else
                    msWord = 0 : lsWord = 0
                End If
            Else
                Dim product As Long = CLng(Agc2Cpu(SignExtend(operand16))) * CLng(Agc2Cpu(SignExtend(other)))
                msWord = CInt((product >> 14) And &H3FFF)
                lsWord = CInt(product And &H3FFF)
                If product < 0 Then
                    msWord = AgcWord.Complement(msWord) And AgcWord.Mask15
                    lsWord = AgcWord.Complement(lsWord) And AgcWord.Mask15
                End If
            End If

            A = msWord And AgcWord.Mask15
            L = lsWord And AgcWord.Mask15
            A16 = 0
        End Sub

        ' ------------------------------------------------------------------
        ' DV
        ' ------------------------------------------------------------------

        Private Sub ExecuteDivide(address10 As Integer, accumulator16 As Integer)
            Dim accPairA As Integer = OverflowCorrected(accumulator16)
            Dim accPairL As Integer = OverflowCorrected(L)

            ' The dividend is the 28-bit double-precision quantity A,L.
            Dim aVal As Integer = AgcToNative(accPairA And AgcWord.Mask15)
            Dim lVal14 As Integer = accPairL And &H3FFF
            Dim lVal As Integer
            If (accPairL And &H4000) <> 0 AndAlso (accPairL And &H3FFF) <> 0 Then
                lVal = -((accPairL And &H3FFF) Xor &H3FFF)
            Else
                lVal = lVal14
            End If
            Dim dividend As Long = CLng(aVal) * 16384L + CLng(lVal)

            ' Fetch the divisor, applying the AGC's sign conventions.
            Dim divisor As Integer
            If address10 = RegA Then
                divisor = A
                If (A And &H4000) = 0 Then divisor = AgcWord.Complement(A)
            ElseIf address10 = RegL Then
                divisor = L
                If ((AbsSP(accPairA) = 0 AndAlso (L And &H4000) <> 0) OrElse
                    (AbsSP(accPairA) <> 0 AndAlso (accPairA And &H4000) <> 0)) Then
                    divisor = AgcWord.Complement(divisor)
                End If
                divisor = SignExtend(OverflowCorrected(AddSP16(divisor, &H4000))) And AgcWord.Mask15
            ElseIf address10 = RegZ Then
                divisor = Z
                If ((AbsSP(accPairA) = 0 AndAlso (L And &H4000) <> 0) OrElse
                    (AbsSP(accPairA) <> 0 AndAlso (accPairA And &H4000) <> 0)) Then
                    divisor = divisor Or &H4000
                End If
            Else
                divisor = ReadAddress10(address10)
            End If
            divisor = OverflowCorrected(divisor) And AgcWord.Mask15

            Dim absA As Integer = AbsSP(accPairA)
            Dim absL As Integer = AbsSP(accPairL)
            Dim divVal As Integer = Agc2Cpu(SignExtend(divisor))
            Dim absK As Integer = Math.Abs(divVal)

            If absK = 0 Then
                ' Division by zero: the hardware saturates to the largest
                ' magnitude of the appropriate sign and leaves L alone.
                If (aVal >= 0) = ((divisor And &H4000) = 0) Then
                    A = &H3FFF
                Else
                    A = &H4000
                End If
                A16 = 0
                Return
            End If

            If absA > absK OrElse (absA = absK AndAlso absL <> 0) OrElse ValueOverflowed(divisor) <> 0 Then
                ' Divisor smaller than the dividend: the quotient cannot be
                ' represented.  Emulate the hardware behaviour (A holds the
                ' maximum-magnitude value with the dividend's sign, L = 0).
                If aVal >= 0 Then A = &H3FFF Else A = &H4000
                A16 = 0
                L = 0
                Return
            End If

            Dim quotient As Long = dividend \ absK
            Dim remainder As Long = dividend Mod absK
            A = NativeToAgc(CInt(quotient)) And AgcWord.Mask15
            A16 = 0
            If remainder = 0 Then
                L = If(dividend >= 0, 0, AgcWord.Mask15)
            Else
                L = NativeToAgc(CInt(remainder)) And AgcWord.Mask15
            End If
            L = SignExtend(OverflowCorrected(L)) And AgcWord.Mask15
        End Sub

    End Class

End Namespace
