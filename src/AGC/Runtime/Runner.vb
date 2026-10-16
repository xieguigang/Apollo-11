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
            If Not asm.Assemble() Then
                Console.WriteLine("Assembly failed with " & asm.Diagnostics.ErrorCount & " errors.")
                Console.WriteLine(asm.Diagnostics.FormatAll(40))
                Return
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
