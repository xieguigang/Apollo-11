' ============================================================================
'  Runner.vb -- the "AGC run" command: assemble a program and execute it on the
'  virtual AGC hardware.  The full virtual hardware lives in the Hardware
'  folder; this module wires the command line to it.
' ============================================================================

Imports System
Imports System.Collections.Generic
Imports System.Globalization

Namespace Agc

    Public Module AgcRunner

        Delegate Function ResolveSourceDirectoryDelegate(arg As String) As String
        Delegate Function CreateAssemblerDelegate(sourceDir As String, verbose As Boolean) As AgcAssembler

        Public Sub Run(rest As List(Of String),
                       resolveSource As ResolveSourceDirectoryDelegate,
                       createAssembler As CreateAssemblerDelegate)

            Dim positional As New List(Of String)()
            For Each a As String In rest
                If Not a.StartsWith("-", StringComparison.Ordinal) Then positional.Add(a)
            Next

            Dim sourceDir As String = resolveSource(If(positional.Count > 0, positional(0), ""))
            If sourceDir = "" Then
                Console.Error.WriteLine("Source directory not found.")
                Return
            End If

            Dim verbose As Boolean = rest.Contains("--verbose")
            Dim asm As AgcAssembler = createAssembler(sourceDir, verbose)

            Console.WriteLine("Assembling " & asm.ProgramName & "...")
            Dim assembled As Boolean = asm.Assemble()
            If Not assembled Then
                Console.WriteLine("Assembly reported " & asm.Diagnostics.ErrorCount & " errors.")
                ' --force runs the rope anyway.  The image is still produced, and
                ' booting it is the only way to find out which errors actually
                ' matter at run time.
                If Not rest.Contains("--force") Then
                    Console.WriteLine("Refusing to run.  Pass --force to run anyway.")
                    Console.WriteLine(asm.Diagnostics.FormatAll(40))
                    Return
                End If
                Console.WriteLine("--force given: running the partial image.")
            End If
            Console.WriteLine("Assembled " & asm.EmittedWords.ToString("N0") & " words.")

            Dim seconds As Double = 1.0
            Dim secText As String = ""
            If TryGetOption(rest, "--seconds", secText) Then
                Double.TryParse(secText, NumberStyles.Float, CultureInfo.InvariantCulture, seconds)
            End If

            Dim machine As New AgcMachine(asm.Image, asm.Symbols, asm.Diagnostics)
            machine.AttachConsole()
            machine.Reset()

            Console.WriteLine("Running for " & seconds.ToString("0.###", CultureInfo.InvariantCulture) & " simulated seconds...")
            If rest.Contains("--find-zero") Then
                ' Boot diagnostics: report the first moment the program counter
                ' reaches 00000, which is the signature of a lost program.
                machine.RunUntilPc(0, CInt(seconds * AgcMachine.CyclesPerSecond))
            End If
            If rest.Contains("--trace-boot") Then
                machine.Cpu.TraceInitialInstructions = 40
            End If
            If rest.Contains("--bank-probe") Then
                machine.Cpu.BankTrace = True
                machine.Cpu.DispatchTrace = 8
                machine.Cpu.TraceSelect = True
            End If
            If rest.Contains("--bank-probe-old") Then
                ' Ground truth for bank selection.  Memory.Fbank is only refreshed at
                ' the end of an instruction, so a step is executed after setting the
                ' registers before anything is read.
                For b As Integer = 0 To 9
                    machine.Memory.Fixed(b, 0) = &H1000 + b
                Next
                machine.Cpu.SetErasable(4, 2 << 10)     ' FBANK = 2
                machine.Cpu.SetErasable(6, 4 << 10)     ' BBANK = 4
                machine.Memory.WriteErasable(4, 2 << 10)
                machine.Memory.WriteErasable(6, 4 << 10)
                machine.Cpu.StepOne()

                ' The hardware bank registers, straight from the mirroring step.
                Console.WriteLine("  FBANK(4)=" & AgcWord.ToOctal(machine.Memory.ReadErasable(4)) &
                                  " BBANK(6)=" & AgcWord.ToOctal(machine.Memory.ReadErasable(6)))
                Console.WriteLine("  hardware Fbank(0)=" & machine.Memory.Fbank(0).ToString() &
                                  " Fbank(1)=" & machine.Memory.Fbank(1).ToString())
                Console.WriteLine("  raw fixed bank 0..9 word0: " &
                                  machine.Memory.Fixed(0, 0).ToString() & " " &
                                  machine.Memory.Fixed(1, 0).ToString() & " " &
                                  machine.Memory.Fixed(2, 0).ToString() & " " &
                                  machine.Memory.Fixed(3, 0).ToString() & " " &
                                  machine.Memory.Fixed(4, 0).ToString() & " " &
                                  machine.Memory.Fixed(5, 0).ToString())
            End If
            If rest.Contains("--read-fixed") Then
                ' Dump the rope words around a fixed address, read through the bank
                ' registers, to see what the CPU will actually fetch.
                Dim startAddr As Integer = &H2526
                Dim v As String = ""
                If TryGetOption(rest, "--read-fixed", v) Then
                    AgcWord.TryParseInteger(v, startAddr)
                End If
                For i As Integer = 0 To 15
                    Console.WriteLine("  fetch[" & AgcWord.ToOctal(startAddr + i) & "] = " &
                                      AgcWord.ToOctal(machine.Memory.ReadFixed(startAddr + i)))
                Next
            End If
            If rest.Contains("--read-bank2") Then
                ' Show what the rope holds at 04000..04010 read through the bank
                ' registers at reset, which is what the first instructions fetch.
                For a As Integer = &H800 To &H80A
                    Console.WriteLine("  fixed[" & AgcWord.ToOctal(a) & "] = " &
                                      AgcWord.ToOctal(machine.Memory.ReadFixed(a)))
                Next
            End If
            machine.RunSeconds(seconds)

            Console.WriteLine()
            Console.WriteLine("Executed " & machine.Cpu.InstructionCount.ToString("N0") & " instructions.")
            Console.WriteLine("AGC time: " & machine.FormatClock())
            machine.PrintSummary()
        End Sub

        Private Function TryGetOption(args As List(Of String), name As String, ByRef value As String) As Boolean
            For i As Integer = 0 To args.Count - 1
                If args(i) = name AndAlso i + 1 < args.Count Then
                    value = args(i + 1)
                    Return True
                End If
            Next
            Return False
        End Function

    End Module

End Namespace
