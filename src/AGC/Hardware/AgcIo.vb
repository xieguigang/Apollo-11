' ============================================================================
'  AgcIo.vb -- the AGC input/output channels and the peripheral devices wired
'  to them: the DSKY, the IMU/CDU counters, the hand controller, the radar and
'  the downlink telemetry stream.
'
'  Channels 0-017 are input/output ports.  Writing an output channel acts on
'  the attached device immediately; reading an input channel returns the
'  latched value driven by the device.
'
'  The 1/3200 second "scaler" that drives TIME1..TIME6 lives here, because the
'  timers are wired to the channel logic in the real machine and the software
'  observes them through the channel interface.
' ============================================================================

Imports System
Imports System.Collections.Generic

Namespace Agc

    Public Class AgcIo

        ' ------------------------------------------------------------------
        ' Channel numbers
        ' ------------------------------------------------------------------

        Public Const ChanScaler1 As Integer = &H20     ' 040
        Public Const ChanScaler2 As Integer = &H21     ' 041
        Public Const ChanCduX As Integer = &H20
        Public Const ChanCduY As Integer = &H21
        Public Const ChanCduZ As Integer = &H22
        Public Const ChanCduT As Integer = &H23
        Public Const ChanCduS As Integer = &H24
        Public Const ChanDsalmout As Integer = &H5       ' 005
        Public Const ChanDsalmout2 As Integer = &H6      ' 006
        Public Const ChanDsky As Integer = &H10          ' 010
        Public Const ChanDsky2 As Integer = &H11         ' 011
        Public Const ChanDsky3 As Integer = &H13         ' 013
        Public Const ChanDsky4 As Integer = &H14         ' 014
        Public Const ChanDskyKm As Integer = &H15        ' 015
        Public Const ChanDskyPro As Integer = &H16       ' 016
        Public Const ChanDskyRset As Integer = &H17      ' 017
        Public Const ChanInlink As Integer = &H30        ' 030
        Public Const ChanOutlink As Integer = &H33       ' 033
        Public Const ChanAltIn As Integer = &H34         ' 034
        Public Const ChanAltOut As Integer = &H35        ' 035
        Public Const ChanRnrrad As Integer = &H36        ' 036

        Private Const NumChannels As Integer = 512

        ' ------------------------------------------------------------------
        ' State
        ' ------------------------------------------------------------------

        ''' <summary>Values seen when reading channels 0-017.</summary>
        Public ReadOnly InputChannel(NumChannels - 1) As Integer

        ''' <summary>Values written to channels 0-017.</summary>
        Public ReadOnly OutputChannel(NumChannels - 1) As Integer

        ''' <summary>The 16 relay rows of channel 010.</summary>
        Public ReadOnly RelayRows(15) As Integer

        Public Property Dsky As AgcDsky
        Public Property Cdu As AgcCdu
        Public Property Downlink As AgcDownlink

        ''' <summary>Scaler accumulator, incremented by 3 every machine cycle.</summary>
        Private _scalerCounter As Integer
        Private Const ScalerDivider As Integer = 3
        Private Const ScalerOverflow As Integer = 80

        ''' <summary>Total number of 1/3200 second ticks since reset.</summary>
        Public Property ScalerTicks As Long

        ''' <summary>True when the program has requested standby.</summary>
        Public Property Standby As Boolean

        ''' <summary>Set when the AGC goes to standby or the night watchman trips.</summary>
        Public ReadOnly PendingRestart As New List(Of String)()

        ''' <summary>Counters for diagnostics.</summary>
        Public Property TelemetryWords As Long
        Public Property ChannelWrites As Long
        Public Property UnprogrammedSequences As Long

        ' The counter registers live in erasable memory; the I/O module needs
        ' access to them for the counter-command channels, so the machine hands
        ' us the memory object.
        Public Property Memory As AgcMemory

        ''' <summary>Callback used to raise interrupts (TIMEnRUPT) from the scaler.</summary>
        Public Property RaiseInterrupt As Action(Of Integer)

        ''' <summary>Callback used to read/write the timer registers.</summary>
        Public Property TimerAccess As ITimerAccess

        Public Sub New()
            Dsky = New AgcDsky()
            Cdu = New AgcCdu()
            Downlink = New AgcDownlink()
        End Sub

        Public Sub Reset()
            Array.Clear(InputChannel, 0, InputChannel.Length)
            Array.Clear(OutputChannel, 0, OutputChannel.Length)
            Array.Clear(RelayRows, 0, RelayRows.Length)
            _scalerCounter = 0
            ScalerTicks = 0
            Standby = False
            PendingRestart.Clear()
            TelemetryWords = 0
            ChannelWrites = 0
            UnprogrammedSequences = 0
            Dsky.Reset()
            Cdu.Reset()
            Downlink.Reset()
            ' At power-up the DSKY key/relay channels read as all ones, which the
            ' software interprets as "no key pressed".
            InputChannel(ChanDskyKm) = AgcWord.Mask15
            InputChannel(ChanDskyPro) = AgcWord.Mask15
            InputChannel(ChanDskyRset) = AgcWord.Mask15
        End Sub

        Public Sub OnRestart()
            PendingRestart.Clear()
        End Sub

        ' ------------------------------------------------------------------
        ' Channel access
        ' ------------------------------------------------------------------

        Public Function ReadChannel(address As Integer) As Integer
            Dim a As Integer = address And &H1FF
            If a >= NumChannels Then Return 0
            RefreshInputs(a)
            Return InputChannel(a) And AgcWord.Mask15
        End Function

        Public Sub WriteChannel(address As Integer, value As Integer)
            Dim a As Integer = address And &H1FF
            If a >= NumChannels Then Return
            ChannelWrites += 1
            value = value And AgcWord.Mask15
            OutputChannel(a) = value

            Select Case a
                Case ChanDsky
                    ' Channel 010 drives the DSKY relay rows.
                    RelayRows((value >> 11) And &H7) = value
                    Dsky.WriteRelayRow((value >> 11) And &H7, value)
                Case ChanDsky2
                    Dsky.WriteChannel011(value)
                Case ChanDsky3
                    Dsky.WriteChannel013(value)
                Case ChanDsky4
                    Dsky.WriteChannel014(value)
                Case ChanOutlink
                    Downlink.SendTelemetry(value)
                    TelemetryWords += 1
                Case ChanAltOut
                    Downlink.SendTelemetry(value)
                    TelemetryWords += 1
                Case &HA
                    ' Channel 010 has a second set of relay rows accessed by an
                    ' EXTEND WRITE; the hardware latches on the high bits.
                    RelayRows((value >> 11) And &H7) = value
                    Dsky.WriteRelayRow((value >> 11) And &H7, value)
                Case Else
                    HandleCounterCommand(a, value)
            End Select
        End Sub

        ' ------------------------------------------------------------------
        ' Programmable counter commands
        ' ------------------------------------------------------------------

        ''' <summary>
        ''' Channels 020-037 carry counter commands.  Bit 15 selects increment
        ''' versus decrement and bit 14 selects the style of increment.
        ''' </summary>
        Private Sub HandleCounterCommand(channel As Integer, value As Integer)
            If TimerAccess Is Nothing Then Return
            Dim pulse As Integer = If((value And &H8000) <> 0, -1, 0)   ' placeholder; decoded below
            Dim up As Boolean = (value And 1) <> 0
            Dim opcode As Integer = (value >> 1) And 7

            Select Case channel
                Case &H20
                    If up Then Cdu.IncrementChannel(CduChan.X, opcode) Else Cdu.DecrementChannel(CduChan.X)
                Case &H21
                    If up Then Cdu.IncrementChannel(CduChan.Y, opcode) Else Cdu.DecrementChannel(CduChan.Y)
                Case &H22
                    If up Then Cdu.IncrementChannel(CduChan.Z, opcode) Else Cdu.DecrementChannel(CduChan.Z)
                Case &H23
                    If up Then Cdu.IncrementChannel(CduChan.T, opcode) Else Cdu.DecrementChannel(CduChan.T)
                Case &H24
                    If up Then Cdu.IncrementChannel(CduChan.S, opcode) Else Cdu.DecrementChannel(CduChan.S)
                Case &H25
                    If up Then Cdu.IncrementChannel(CduChan.Y, opcode) Else Cdu.DecrementChannel(CduChan.Y)
                Case &H26
                    If up Then Cdu.IncrementChannel(CduChan.Z, opcode) Else Cdu.DecrementChannel(CduChan.Z)
                Case &H27
                    If up Then Cdu.IncrementChannel(CduChan.T, opcode) Else Cdu.DecrementChannel(CduChan.T)
                Case Else
                    ' Not a counter channel.
            End Select
        End Sub

        ' ------------------------------------------------------------------
        ' Scaler / timers
        ' ------------------------------------------------------------------

        ''' <summary>Called once per executed instruction.</summary>
        Public Sub OnCycleEnd(cycleCount As Long)
            _scalerCounter += ScalerDivider
            While _scalerCounter >= ScalerOverflow
                _scalerCounter -= ScalerOverflow
                ScalerTicks += 1
                Dim ch1 As Integer = (InputChannel(ChanScaler1) + 1) And &H7FFF
                InputChannel(ChanScaler1) = ch1
                If ch1 = 0 Then
                    InputChannel(ChanScaler2) = (InputChannel(ChanScaler2) + 1) And &H3FFF
                End If
                TickTimers(ch1)
            End While
        End Sub

        Private Sub TickTimers(scaler1 As Integer)
            If TimerAccess Is Nothing Then Return
            Dim low5 As Integer = scaler1 And &H1F

            ' TIME1 and TIME3 advance every 10 ms.
            If low5 = &H10 Then
                If TimerAccess.CounterPINC(AgcCpu.RegTIME1) Then
                    TimerAccess.CounterPINC(AgcCpu.RegTIME2)
                End If
                If TimerAccess.CounterPINC(AgcCpu.RegTIME3) Then
                    If RaiseInterrupt IsNot Nothing Then RaiseInterrupt.Invoke(3)     ' T3RUPT
                End If
            End If

            ' TIME5 is 5 ms out of phase with TIME3.
            If low5 = 0 Then
                If TimerAccess.CounterPINC(AgcCpu.RegTIME5) Then
                    If RaiseInterrupt IsNot Nothing Then RaiseInterrupt.Invoke(2)     ' T5RUPT
                End If
            End If

            ' TIME4 is 7.5 ms out of phase.
            If low5 = 8 Then
                If TimerAccess.CounterPINC(AgcCpu.RegTIME4) Then
                    If RaiseInterrupt IsNot Nothing Then RaiseInterrupt.Invoke(4)     ' T4RUPT
                End If
            End If

            ' TIME6 counts down when enabled by channel 013 bit 15.
            If (InputChannel(ChanDsky3) And &H4000) <> 0 AndAlso (scaler1 And 1) = 1 Then
                If TimerAccess.CounterDINC(AgcCpu.RegTIME6) Then
                    If RaiseInterrupt IsNot Nothing Then RaiseInterrupt.Invoke(1)     ' T6RUPT
                    InputChannel(ChanDsky3) = InputChannel(ChanDsky3) And &H3FFF
                End If
            End If
        End Sub

        ''' <summary>1's-complement increment used by the scaler.</summary>
        Public Function CounterPINC(address As Integer) As Boolean
            If TimerAccess Is Nothing Then Return False
            Return TimerAccess.CounterPINC(address)
        End Function

        ' ------------------------------------------------------------------
        ' Input refresh
        ' ------------------------------------------------------------------

        Private Sub RefreshInputs(channel As Integer)
            Select Case channel
                Case ChanDskyKm
                    Dim km As Integer = Dsky.ReadKeys()
                    InputChannel(ChanDskyKm) = km
                    InputChannel(ChanDskyPro) = Dsky.ProWord
                    InputChannel(ChanDskyRset) = Dsky.RsetWord
                Case ChanScaler1, ChanScaler2
                    ' Driven by the scaler.
                Case Else
                    ' Leave other channels as the devices set them.
            End Select
        End Sub

        ''' <summary>Called once per instruction to mirror bank registers into channels.</summary>
        Public Sub UpdateBankIO(regEBank As Integer, regFBank As Integer, regBBank As Integer, eBank As Integer)
            ' Channel 7 carries the super-bank selection and various flags.
            Dim ch7 As Integer = InputChannel(7)
            ch7 = ch7 And &H6000
            ch7 = ch7 Or ((regFBank >> 10) And &H37)
            InputChannel(7) = ch7 And AgcWord.Mask15
        End Sub

    End Class

    ''' <summary>Interface used by the I/O module to reach the timer registers.</summary>
    Public Interface ITimerAccess
        ''' <summary>1's-complement increment; returns True on overflow.</summary>
        Function CounterPINC(address As Integer) As Boolean
        ''' <summary>Diminishing increment; returns True when the counter reached zero.</summary>
        Function CounterDINC(address As Integer) As Boolean
    End Interface

End Namespace
