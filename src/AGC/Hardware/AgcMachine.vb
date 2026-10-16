' ============================================================================
'  AgcMachine.vb -- the assembled virtual AGC: memory, processor, channels and
'  peripherals, plus the clock that ties them together.
'
'  One "instruction" is executed per Step().  The machine tracks simulated time
'  from the machine-cycle counter (11.72 microseconds per machine cycle), which
'  is what the AGC software's clock routines measure.
' ============================================================================

Imports System
Imports System.Collections.Generic
Imports System.Text

Namespace Agc

    Public Class AgcMachine
        Implements ITimerAccess

        ''' <summary>Seconds per machine cycle (the AGC's 2.048 MHz clock / 12 phases).</summary>
        Public Const SecondsPerCycle As Double = 1.0 / 85333.3333

        ''' <summary>Machine cycles per second of AGC time.</summary>
        Public Const CyclesPerSecond As Double = 85333.3333

        Public ReadOnly Image As AgcRopeImage
        Public ReadOnly Symbols As AgcSymbolTable
        Public ReadOnly Memory As AgcMemory
        Public ReadOnly Cpu As AgcCpu
        Public ReadOnly Io As AgcIo
        Public ReadOnly Diagnostics As AgcDiagnostics

        Public Property Tracer As Action(Of AgcCpu)

        ''' <summary>Number of times the program has been restarted (GOJAM).</summary>
        Public ReadOnly Property RestartCount As Integer
            Get
                Return Cpu.CpuRestartCount
            End Get
        End Property

        Public Sub New(image As AgcRopeImage, symbols As AgcSymbolTable, diagnostics As AgcDiagnostics)
            Me.Image = image
            Me.Symbols = symbols
            Me.Diagnostics = diagnostics

            Memory = New AgcMemory(image)
            Io = New AgcIo()
            Io.Memory = Memory
            Io.TimerAccess = Me
            Cpu = New AgcCpu(Memory, Io)

            Io.RaiseInterrupt = AddressOf OnInterruptRequest
        End Sub

        ''' <summary>Load the rope image into memory.</summary>
        Public Sub AttachConsole()
            Memory.Load()
        End Sub

        Public Sub Reset()
            Memory.Load()
            Memory.EBank = 0
            Memory.Fbank(0) = 0
            Memory.Fbank(1) = 0
            Cpu.Reset()
            If Tracer IsNot Nothing Then Cpu.Tracer = Tracer
        End Sub

        Public Sub StepOne()
            Cpu.StepOne()
        End Sub

        ''' <summary>Run for a number of simulated seconds.</summary>
        Public Sub RunSeconds(seconds As Double)
            Dim target As Long = Cpu.CycleCount + CLng(seconds * CyclesPerSecond)
            While Cpu.CycleCount < target AndAlso Not Cpu.Halted
                Cpu.StepOne()
            End While
        End Sub

        ''' <summary>Run for a number of machine cycles.</summary>
        Public Sub RunCycles(cycles As Long)
            Dim target As Long = Cpu.CycleCount + cycles
            While Cpu.CycleCount < target AndAlso Not Cpu.Halted
                Cpu.StepOne()
            End While
        End Sub

        ''' <summary>Run until the predicate returns True, or the cycle budget runs out.</summary>
        Public Function RunUntil(predicate As Func(Of AgcMachine, Boolean), maxCycles As Long) As Boolean
            Dim target As Long = Cpu.CycleCount + maxCycles
            While Cpu.CycleCount < target AndAlso Not Cpu.Halted
                If predicate(Me) Then Return True
                Cpu.StepOne()
            End While
            Return predicate(Me)
        End Function

        ''' <summary>Simulated seconds elapsed since reset.</summary>
        Public ReadOnly Property ElapsedSeconds As Double
            Get
                Return Cpu.CycleCount / CyclesPerSecond
            End Get
        End Property

        ''' <summary>Format the AGC clock (TIME1/TIME2 counts of 10 ms).</summary>
        Public Function FormatClock() As String
            Dim t1 As Integer = Cpu.GetErasable(AgcCpu.RegTIME1)
            Dim t2 As Integer = Cpu.GetErasable(AgcCpu.RegTIME2)
            Dim ticks As Double = AgcCpu.AgcToNative(t2) * 16384.0 + AgcCpu.AgcToNative(t1)
            Dim seconds As Double = ticks * 0.01
            Dim hrs As Integer = CInt(Math.Floor(seconds / 3600.0))
            Dim mins As Integer = CInt(Math.Floor((seconds - hrs * 3600.0) / 60.0))
            Dim secs As Double = seconds - hrs * 3600.0 - mins * 60.0
            Return String.Format("{0:00}:{1:00}:{2:00.00}", hrs, mins, secs)
        End Function

        ''' <summary>Look up a symbol's pseudo address by name.</summary>
        Public Function Lookup(name As String) As Integer
            Dim sym As AgcSymbol = Nothing
            If Symbols IsNot Nothing AndAlso Symbols.TryGetValue(name, sym) Then
                Return sym.Address.PseudoValue
            End If
            Return -1
        End Function

        Public Sub PrintSummary()
            Console.WriteLine("Machine cycles: " & Cpu.CycleCount.ToString("N0") &
                              "  (simulated time " & ElapsedSeconds.ToString("0.000") & " s)")
            Console.WriteLine("Restarts: " & Cpu.CpuRestartCount.ToString() &
                              "   Halted: " & Cpu.Halted.ToString())
            If Cpu.Halted Then Console.WriteLine("Halt reason: " & Cpu.HaltReason)
            Console.WriteLine("Telemetry words: " & Io.TelemetryWords.ToString("N0") &
                              "   Channel writes: " & Io.ChannelWrites.ToString("N0"))
            Console.WriteLine("AGC clock (TIME1/TIME2): " & FormatClock())
        End Sub

        ' ------------------------------------------------------------------
        ' ITimerAccess -- the timer registers live in erasable memory
        ' ------------------------------------------------------------------

        Private Function TimerCounterPINC(address As Integer) As Boolean Implements ITimerAccess.CounterPINC
            Dim v As Integer = Cpu.GetErasable(address)
            If v = &H3FFF Then
                Cpu.SetErasable(address, 0)
                Return True
            End If
            v = (v + 1) And AgcWord.Mask15
            If v = 0 Then v = 1        ' account for the -0 to +1 transition
            Cpu.SetErasable(address, v)
            Return False
        End Function

        Private Function TimerCounterDINC(address As Integer) As Boolean Implements ITimerAccess.CounterDINC
            Dim v As Integer = Cpu.GetErasable(address)
            If v = 0 OrElse v = AgcWord.Mask15 Then Return True
            If (v And &H4000) <> 0 Then
                v = AgcCpu.AddSP16(AgcCpu.SignExtend(v), 1) And AgcWord.Mask15
            Else
                v = AgcCpu.AddSP16(AgcCpu.SignExtend(v), AgcCpu.SignExtend(AgcWord.Mask15)) And AgcWord.Mask15
            End If
            Cpu.SetErasable(address, v)
            Return False
        End Function

        Private Sub OnInterruptRequest(priority As Integer)
            If priority >= 1 AndAlso priority <= 10 Then
                Cpu.InterruptRequests(priority) = True
            End If
        End Sub

    End Class

End Namespace
