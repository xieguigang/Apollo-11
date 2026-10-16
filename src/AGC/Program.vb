' ============================================================================
'  Program.vb -- entry point for the VB.NET AGC assembler + virtual hardware.
'
'  Usage:
'      AGC assemble <source-dir> [options]      assemble an AGC program
'      AGC run      <source-dir> [options]      assemble and execute it
'      AGC info     <source-dir>                report source statistics
'      AGC selftest                             run assembler unit tests
' ============================================================================

Imports System
Imports System.Collections.Generic
Imports System.IO
Imports System.Text

Namespace Agc

    Module Program

        Private _repoRoot As String = ""

        Private Function FindRepoRoot() As String
            If _repoRoot <> "" Then Return _repoRoot

            ' Walk up from the executable looking for the apollo-11 folder.
            Dim dir As String = AppContext.BaseDirectory
            Dim candidate As String = ""
            For i As Integer = 1 To 8
                Dim probe As String = Path.Combine(dir, "apollo-11")
                If Directory.Exists(probe) Then
                    candidate = dir
                    Exit For
                End If
                Dim parent As DirectoryInfo = Directory.GetParent(dir)
                If parent Is Nothing Then Exit For
                dir = parent.FullName
            Next

            If candidate = "" Then
                ' Also try the current working directory tree.
                dir = Directory.GetCurrentDirectory()
                For i As Integer = 1 To 6
                    Dim probe As String = Path.Combine(dir, "apollo-11")
                    If Directory.Exists(probe) Then
                        candidate = dir
                        Exit For
                    End If
                    Dim parent As DirectoryInfo = Directory.GetParent(dir)
                    If parent Is Nothing Then Exit For
                    dir = parent.FullName
                Next
            End If

            _repoRoot = candidate
            Return _repoRoot
        End Function

        Private Function DefaultSourceDirectory(programName As String) As String
            Dim root As String = FindRepoRoot()
            If root = "" Then Return ""
            Return Path.Combine(Path.Combine(root, "apollo-11"), programName)
        End Function

        Private Function CreateAssembler(sourceDir As String, verbose As Boolean) As AgcAssembler
            Dim asm As New AgcAssembler()
            asm.SourceDirectory = sourceDir
            asm.MainFile = "MAIN.agc"
            asm.Verbose = verbose
            asm.ProgramName = New DirectoryInfo(sourceDir).Name
            Return asm
        End Function

        Private Function ResolveSourceDirectory(arg As String) As String
            If Not String.IsNullOrEmpty(arg) Then
                If Directory.Exists(arg) Then Return Path.GetFullPath(arg)
                Dim d As String = DefaultSourceDirectory(arg)
                If d <> "" AndAlso Directory.Exists(d) Then Return d
                Return ""
            End If
            Return DefaultSourceDirectory("Comanche055")
        End Function

        Sub Main(args As String())
            If args.Length = 0 Then
                PrintUsage()
                Return
            End If

            Dim command As String = args(0).ToLowerInvariant()
            Dim rest As New List(Of String)(args)
            rest.RemoveAt(0)

            Select Case command
                Case "assemble", "asm"
                    CommandAssemble(rest)
                Case "run"
                    CommandRun(rest)
                Case "info"
                    CommandInfo(rest)
                Case "selftest"
                    CommandSelfTest()
                Case "smoke"
                    CommandSmoke()
                Case "compare"
                    CommandCompare(rest)
                Case "probe"
                    DebugProbe.DumpSynthetic()
                    If rest.Count > 0 Then
                        DebugProbe.DumpLines(rest(0), New Integer() {32, 34, 36, 37, 92})
                    End If
                Case "bankdump"
                    CommandBankDump(rest)
                Case "dump"
                    CommandDump(rest)
                Case "-h", "--help", "help", "/?"
                    PrintUsage()
                Case Else
                    Console.Error.WriteLine("Unknown command '" & args(0) & "'.")
                    PrintUsage()
            End Select
        End Sub

        Private Sub PrintUsage()
            Console.WriteLine("AGC assembler and virtual hardware -- VB.NET implementation")
            Console.WriteLine()
            Console.WriteLine("  AGC assemble <source-dir> [--verbose] [--out FILE] [--symbols FILE]")
            Console.WriteLine("        Assemble the AGC program in <source-dir> (which must contain MAIN.agc).")
            Console.WriteLine()
            Console.WriteLine("  AGC run <source-dir> [--verbose] [--seconds N] [--words N]")
            Console.WriteLine("        Assemble the program and execute it on the virtual AGC hardware.")
            Console.WriteLine()
            Console.WriteLine("  AGC info <source-dir>")
            Console.WriteLine("        Report source statistics without assembling.")
            Console.WriteLine()
            Console.WriteLine("  AGC dump <source-dir> --out FILE")
            Console.WriteLine("        Write the assembled rope image in virtualagc .binsource format.")
            Console.WriteLine()
            Console.WriteLine("  AGC selftest")
            Console.WriteLine("        Run the built-in assembler self tests.")
            Console.WriteLine()
            Console.WriteLine("Source directories: Comanche055 (Command Module) and Luminary099 (Lunar Module).")
            Dim root As String = FindRepoRoot()
            If root <> "" Then
                Console.WriteLine("Detected repository root: " & root)
                Console.WriteLine("  " & Path.Combine(root, "apollo-11", "Comanche055"))
                Console.WriteLine("  " & Path.Combine(root, "apollo-11", "Luminary099"))
            End If
        End Sub

        ' ------------------------------------------------------------------
        ' assemble
        ' ------------------------------------------------------------------

        Private Function ParseOption(rest As List(Of String), name As String, ByRef value As String) As Boolean
            For i As Integer = 0 To rest.Count - 1
                If rest(i) = name Then
                    If i + 1 < rest.Count Then
                        value = rest(i + 1)
                        Return True
                    End If
                End If
            Next
            Return False
        End Function

        Private Function HasFlag(rest As List(Of String), name As String) As Boolean
            Return rest.Contains(name)
        End Function

        Private Sub CommandAssemble(rest As List(Of String))
            Dim positional As New List(Of String)()
            For Each a As String In rest
                If Not a.StartsWith("-", StringComparison.Ordinal) Then positional.Add(a)
            Next
            Dim sourceDir As String = ResolveSourceDirectory(If(positional.Count > 0, positional(0), ""))
            If sourceDir = "" Then
                Console.Error.WriteLine("Source directory not found.")
                Return
            End If

            Dim verbose As Boolean = HasFlag(rest, "--verbose")
            Dim asm As AgcAssembler = CreateAssembler(sourceDir, verbose)

            Console.WriteLine("Assembling " & asm.ProgramName & " from " & sourceDir)
            Dim sw As System.Diagnostics.Stopwatch = System.Diagnostics.Stopwatch.StartNew()
            Dim ok As Boolean = asm.Assemble()
            sw.Stop()

            PrintBankUsage(asm)
            Console.WriteLine()
            Console.WriteLine("Lines: " & asm.TotalAssembledLines.ToString("N0") &
                              "   Words: " & asm.EmittedWords.ToString("N0") &
                              "   Symbols: " & asm.Symbols.Count.ToString("N0") &
                              "   Time: " & sw.ElapsedMilliseconds.ToString("N0") & " ms")
            Console.WriteLine("Errors: " & asm.Diagnostics.ErrorCount &
                              "   Warnings: " & asm.Diagnostics.WarningCount)

            If asm.Diagnostics.Messages.Count > 0 Then
                Console.WriteLine()
                Console.WriteLine(asm.Diagnostics.FormatAll(60))
            End If

            Dim outFile As String = ""
            If ParseOption(rest, "--out", outFile) Then
                File.WriteAllText(outFile, asm.Image.ToBinSource(False))
                Console.WriteLine("Rope image written to " & outFile)
            End If

            Dim symFile As String = ""
            If ParseOption(rest, "--symbols", symFile) Then
                WriteSymbolFile(asm, symFile)
                Console.WriteLine("Symbol table written to " & symFile)
            End If

            Console.WriteLine(If(ok, "ASSEMBLY OK", "ASSEMBLY FAILED"))
        End Sub

        Private Sub PrintBankUsage(asm As AgcAssembler)
            Console.WriteLine()
            Console.WriteLine("Fixed memory usage (words used / 1024):")
            Dim line As New StringBuilder()
            Dim count As Integer = 0
            For b As Integer = 0 To AgcRopeImage.FixedBankCount - 1
                Dim used As Integer = asm.UsedInBank(b)
                If used = 0 Then Continue For
                line.Append(String.Format("{0:00}:{1,5}  ", b, used))
                count += 1
                If count Mod 8 = 0 Then
                    Console.WriteLine("  " & line.ToString())
                    line.Clear()
                End If
            Next
            If line.Length > 0 Then Console.WriteLine("  " & line.ToString())
        End Sub

        Private Sub WriteSymbolFile(asm As AgcAssembler, path As String)
            Dim sb As New StringBuilder()
            sb.AppendLine("# Symbol table for " & asm.ProgramName)
            For Each s As AgcSymbol In asm.Symbols.SortedSymbols()
                sb.AppendLine(String.Format("{0,-24} {1,-16} {2}", s.Name, s.Address.ToString(), s.Address.PseudoValue.ToString()))
            Next
            File.WriteAllText(path, sb.ToString())
        End Sub

        ' ------------------------------------------------------------------
        ' info
        ' ------------------------------------------------------------------

        Private Sub CommandInfo(rest As List(Of String))
            Dim sourceDir As String = ResolveSourceDirectory(If(rest.Count > 0, rest(0), ""))
            If sourceDir = "" Then
                Console.Error.WriteLine("Source directory not found.")
                Return
            End If

            Dim reader As New AgcSourceReader(sourceDir, New AgcDiagnostics())
            Dim lines As List(Of AgcLine) = reader.Read(Path.Combine(sourceDir, "MAIN.agc"))
            Dim ops As New Dictionary(Of String, Integer)(StringComparer.Ordinal)
            For Each l As AgcLine In lines
                If String.IsNullOrEmpty(l.Opcode) Then Continue For
                Dim n As Integer = 0
                ops.TryGetValue(l.Opcode, n)
                ops(l.Opcode) = n + 1
            Next
            Console.WriteLine("Source directory : " & sourceDir)
            Console.WriteLine("Lines            : " & lines.Count.ToString("N0"))
            Console.WriteLine("Distinct opcodes : " & ops.Count.ToString("N0"))
        End Sub

        ' ------------------------------------------------------------------
        ' dump
        ' ------------------------------------------------------------------

        Private Sub CommandDump(rest As List(Of String))
            Dim outFile As String = ""
            If Not ParseOption(rest, "--out", outFile) Then
                Console.Error.WriteLine("dump requires --out FILE")
                Return
            End If
            Dim positional As New List(Of String)()
            For Each a As String In rest
                If Not a.StartsWith("-", StringComparison.Ordinal) Then positional.Add(a)
            Next
            Dim sourceDir As String = ResolveSourceDirectory(If(positional.Count > 0, positional(0), ""))
            If sourceDir = "" Then Return
            Dim asm As AgcAssembler = CreateAssembler(sourceDir, False)
            asm._traceLines = 12
            asm.Assemble()
            File.WriteAllText(outFile, asm.Image.ToBinSource(False))
            Console.WriteLine("Wrote " & outFile)
        End Sub

        ' ------------------------------------------------------------------
        ' run / selftest are implemented in their own modules
        ' ------------------------------------------------------------------

        Private Sub CommandRun(rest As List(Of String))
            AgcRunner.Run(rest, AddressOf ResolveSourceDirectory, AddressOf CreateAssembler)
        End Sub

        Private Sub CommandSelfTest()
            Dim failures As Integer = AgcSelfTest.RunAll()
            If failures = 0 Then
                Console.WriteLine("All self tests passed.")
            Else
                Console.WriteLine(failures.ToString() & " self test(s) failed.")
            End If
        End Sub

        ''' <summary>Run the end-to-end assembler + virtual hardware smoke test.</summary>
        Private Sub CommandSmoke()
            Dim failures As Integer = AgcSmokeTest.Run()
            If failures > 0 Then Environment.ExitCode = 1
        End Sub

        ''' <summary>Development aid: print the first words of selected banks.</summary>
        Private Sub CommandBankDump(rest As List(Of String))
            Dim positional As New List(Of String)()
            For Each a As String In rest
                If Not a.StartsWith("-", StringComparison.Ordinal) Then positional.Add(a)
            Next
            If positional.Count < 1 Then
                Console.Error.WriteLine("bankdump requires <source-dir>")
                Return
            End If
            Dim sourceDir As String = ResolveSourceDirectory(positional(0))
            If sourceDir = "" Then Return
            Dim asm As AgcAssembler = CreateAssembler(sourceDir, False)
            asm._traceLines = 12
            asm.Assemble()
            For Each bank As Integer In New Integer() {0, 1, 2, 3, 4}
                Dim sb As New StringBuilder()
                sb.Append("bank ").Append(bank.ToString()).Append(": ")
                For o As Integer = 0 To 7
                    sb.Append(AgcWord.ToOctal(asm.Image.Fixed(bank, o))).Append(" ")
                Next
                sb.Append(" used=").Append(asm.Image.FixedWordCount(bank).ToString())
                sb.Append(" hw=").Append(asm.Image.FixedHighWater(bank).ToString())
                Console.WriteLine(sb.ToString())
            Next
            Console.WriteLine("written=" & asm.Image.WrittenCount.ToString())
        End Sub

        ''' <summary>
        ''' Assemble a program and compare the rope image against a published
        ''' ".binsource" octal listing.
        ''' </summary>
        Private Sub CommandCompare(rest As List(Of String))
            Dim positional As New List(Of String)()
            For Each a As String In rest
                If Not a.StartsWith("-", StringComparison.Ordinal) Then positional.Add(a)
            Next
            If positional.Count < 2 Then
                Console.Error.WriteLine("compare requires <source-dir> <binsource-file>")
                Return
            End If

            Dim sourceDir As String = ResolveSourceDirectory(positional(0))
            If sourceDir = "" Then
                Console.Error.WriteLine("Source directory not found: " & positional(0))
                Return
            End If
            Dim referenceFile As String = positional(1)
            If Not File.Exists(referenceFile) Then
                Console.Error.WriteLine("Reference file not found: " & referenceFile)
                Return
            End If

            Dim asm As AgcAssembler = CreateAssembler(sourceDir, False)
            Console.WriteLine("Assembling " & asm.ProgramName & "...")
            asm._traceLines = 12
            asm.Assemble()
            Console.WriteLine("Assembled " & asm.EmittedWords.ToString("N0") & " words, " &
                              asm.Diagnostics.ErrorCount.ToString("N0") & " errors.")

            Dim source As New AgcBinSource()
            source.Load(referenceFile)
            Console.WriteLine("Reference has " & source.BankCount().ToString() & " banks.")
            If source.Errors.Count > 0 Then
                Console.WriteLine("Reference parse problems: " & source.Errors.Count.ToString())
                For i As Integer = 0 To Math.Min(3, source.Errors.Count - 1)
                    Console.WriteLine("  " & source.Errors(i))
                Next
            End If

            Dim cmp As New AgcRopeComparer()
            cmp.Compare(asm.Image, source)

            Console.WriteLine()
            Console.WriteLine("Words matching the reference : " & cmp.SameCount.ToString("N0"))
            Console.WriteLine("Words differing             : " & cmp.DifferCount.ToString("N0"))
            If cmp.SameCount + cmp.DifferCount > 0 Then
                Dim pct As Double = 100.0 * cmp.SameCount / (cmp.SameCount + cmp.DifferCount)
                Console.WriteLine("Match rate                  : " & pct.ToString("0.00") & " %")
            End If
            If cmp.Mismatches.Count > 0 Then
                Console.WriteLine()
                Console.WriteLine("First mismatches:")
                For Each m As String In cmp.Mismatches
                    Console.WriteLine("  " & m)
                Next
            End If
        End Sub

    End Module

End Namespace
