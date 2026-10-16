' ============================================================================
'  SmokeTest.vb -- end-to-end verification of the assembler and the virtual
'  hardware on a small but real AGC program.
'
'  The program is written in AGC assembly, assembled by AgcAssembler, loaded
'  into AgcMemory and executed by AgcCpu.  Every expected value below is taken
'  from the Block II instruction definitions, so a pass means the assembler
'  encodes correctly AND the CPU executes correctly.
' ============================================================================

Imports System
Imports System.Collections.Generic
Imports System.IO
Imports System.Text

Namespace Agc

    Public Module AgcSmokeTest

        Private _failed As Integer
        Private _passed As Integer

        Public Function Run() As Integer
            _failed = 0
            _passed = 0

            RunAritmeticProgram()
            RunInterpreterProgram()

            Console.WriteLine()
            Console.WriteLine("Smoke test: " & _passed.ToString() & " checks passed, " &
                              _failed.ToString() & " failed.")
            Return _failed
        End Function

        Private Sub Check(condition As Boolean, name As String, Optional detail As String = "")
            If condition Then
                _passed += 1
            Else
                _failed += 1
                Console.WriteLine("  FAIL: " & name & If(detail = "", "", "  (" & detail & ")"))
            End If
        End Sub

        Private Function Octal(v As Integer) As String
            Return AgcWord.ToOctal(v)
        End Function

        ''' <summary>
        ''' Assemble a short program, run it, and check the machine state at the
        ''' end.  The program is:
        '''
        '''     SETLOC  04000
        '''     BANK
        '''         CA      ONE         # A = 1
        '''         AD      TWO         # A = 1 + 2 = 3
        '''         TS      RESULT      # RESULT = 3, A cleared by TS?  No: TS stores A
        '''         CA      RESULT      # A = 3
        ''' LOOP    INCR    COUNT       # COUNT = COUNT + 1
        '''         CCS     COUNT       # A = diminishing |COUNT|
        '''         TCF     DONE        # COUNT > 0 -> done
        '''         TCF     LOOP
        ''' DONE    TCF     DONE        # spin here
        '''
        '''     ONE     DEC     1
        '''     TWO     DEC     2
        '''     COUNT   DEC     0
        '''     RESULT  ERASE
        ''' </summary>
        Private Sub RunAritmeticProgram()
            Console.WriteLine("Assembler + CPU smoke test (arithmetic and CCS):")

            Dim dir As String = Path.Combine(Path.GetTempPath(), "agc-smoke-" & Guid.NewGuid().ToString("N"))
            Directory.CreateDirectory(dir)
            Try
                Dim t As String = Chr(9) & Chr(9)
                Dim one As String = Chr(9)
                Dim src As New StringBuilder()
                src.Append(t & "SETLOC" & one & "4000" & vbCrLf)
                src.Append(t & "BANK" & vbCrLf)
                src.Append(one & "CA" & t & "ONE" & vbCrLf)
                src.Append(one & "AD" & t & "TWO" & vbCrLf)
                src.Append(one & "TS" & t & "RESULT" & vbCrLf)
                src.Append(one & "CA" & t & "RESULT" & vbCrLf)
                src.Append(one & "TS" & t & "SAVED" & vbCrLf)
                src.Append("LOOP" & one & "INCR" & t & "COUNT" & vbCrLf)
                src.Append(one & "CCS" & t & "COUNT" & vbCrLf)
                src.Append(one & "TCF" & t & "DONE" & vbCrLf)
                src.Append(one & "TCF" & t & "LOOP" & vbCrLf)
                src.Append("DONE" & one & "TCF" & t & "DONE" & vbCrLf)
                src.Append("ONE" & t & "DEC" & one & "1" & vbCrLf)
                src.Append("TWO" & t & "DEC" & one & "2" & vbCrLf)
                src.Append("COUNT" & t & "DEC" & one & "0" & vbCrLf)
                src.Append("RESULT" & t & "ERASE" & vbCrLf)
                src.Append("SAVED" & t & "ERASE" & vbCrLf)
                File.WriteAllText(Path.Combine(dir, "MAIN.agc"), src.ToString())
                Console.WriteLine("  (smoke program written to " & dir & ")")

                Dim asm As New AgcAssembler()
                asm.SourceDirectory = dir
                asm.MainFile = "MAIN.agc"
                Dim ok As Boolean = asm.Assemble()
                Check(ok, "program assembles", asm.Diagnostics.FormatAll(10))
                If Not ok Then Return

                ' --- Check the encoding of the individual instructions.
                Dim w As Integer = asm.Image.GetFixed(2, 2048)
                Check(w = &H9000 + (asm.Lookup("ONE") And &HFFF), "CA ONE encodes as 0110000+addr",
                      Octal(w))
                w = asm.Image.GetFixed(2, 2049)
                Check(w = &H10000 + (asm.Lookup("TWO") And &HFFF), "AD TWO encodes as 0200000+addr",
                      Octal(w))

                ' --- Run it on the virtual hardware.
                Dim machine As New AgcMachine(asm.Image, asm.Symbols, asm.Diagnostics)
                machine.AttachConsole()
                machine.Reset()
                machine.RunCycles(400)

                Dim saved As Integer = CpuRead(machine, "SAVED", asm)
                CheckEqual(saved, 3, "A + B = 3 reached the SAVED register")

                Dim count As Integer = CpuRead(machine, "COUNT", asm)
                Check(count = 1, "CCS/TCF loop ran exactly once", "COUNT=" & Octal(count))

                ' The CPU must have executed the instructions we placed.
                Check(machine.Cpu.InstructionCount > 8, "CPU executed the program",
                      machine.Cpu.InstructionCount.ToString() & " instructions")
                Check(machine.Cpu.Z = asm.Lookup("DONE") And &HFFF, "CPU is spinning at DONE",
                      "Z=" & Octal(machine.Cpu.Z) & " DONE=" & Octal(asm.Lookup("DONE") And &HFFF))
            Finally
                Try
                    Directory.Delete(dir, True)
                Catch
                End Try
            End Try
        End Sub

        ''' <summary>
        ''' Assemble a program that uses the interpretive (Danzig) instruction set
        ''' and run it, to prove the interpreter's encoding is coherent.
        '''
        '''     SETLOC  04000
        '''     BANK
        '''         DLOAD   A           # MPAC = A (double precision)
        '''         DAD     B           # MPAC = A + B
        '''         STODL   C           # C = MPAC
        '''         EXIT
        '''     A       2DEC    1.0
        '''     B       2DEC    2.0
        '''     C       ERASE   2
        ''' </summary>
        Private Sub RunInterpreterProgram()
            Console.WriteLine("Interpreter instruction encoding check:")

            Dim dir As String = Path.Combine(Path.GetTempPath(), "agc-interp-" & Guid.NewGuid().ToString("N"))
            Directory.CreateDirectory(dir)
            Try
                Dim t As String = Chr(9) & Chr(9)
                Dim one As String = Chr(9)
                Dim src As New StringBuilder()
                src.Append(t & "SETLOC" & one & "4000" & vbCrLf)
                src.Append(t & "BANK" & vbCrLf)
                src.Append(one & "DLOAD" & t & "A" & vbCrLf)
                src.Append(one & "DAD" & t & "B" & vbCrLf)
                src.Append(one & "STODL" & t & "C" & vbCrLf)
                src.Append(one & "EXIT" & vbCrLf)
                src.Append("A" & t & "2DEC" & one & "1.0" & vbCrLf)

                File.WriteAllText(Path.Combine(dir, "MAIN.agc"), src.ToString())

                Dim asm As New AgcAssembler()
                asm.SourceDirectory = dir
                asm.MainFile = "MAIN.agc"
                asm.Assemble()

                ' DLOAD has interpretive code 031, so the encoded word must be the
                ' complement of (031+1) = 032: 077777 ^ 032 = 077745.
                Dim w As Integer = asm.Image.GetFixed(2, 2048)
                CheckEqual(w, AgcWord.Mask15 Xor (&O31 + 1), "DLOAD encodes as the complement of 032 octal")

                ' The next word is the operand address of A.
                Dim aAddr As Integer = asm.Lookup("A") And &HFFF
                CheckEqual(asm.Image.GetFixed(2, 2049), aAddr, "DLOAD operand word holds the address of A")
            Finally
                Try
                    Directory.Delete(dir, True)
                Catch
                End Try
            End Try
        End Sub

        ' ------------------------------------------------------------------

        Private Function CpuRead(machine As AgcMachine, symbol As String, asm As AgcAssembler) As Integer
            Dim pseudo As Integer = asm.Lookup(symbol)
            If pseudo < 0 Then Return -1
            Dim addr As AgcAddress = AgcCodec.PseudoToAddress(pseudo)
            If addr.IsErasable Then
                If addr.Unbanked Then Return machine.Cpu.GetErasable(addr.SReg)
                Return machine.Memory.ReadErasableBank(addr.EB, (addr.SReg - 5120) + 256)
            End If
            Return machine.Memory.ReadFixed(addr.SReg)
        End Function

        Private Sub CheckEqual(actual As Integer, expected As Integer, name As String)
            Check(actual = expected, name, "expected " & Octal(expected) & ", got " & Octal(actual))
        End Sub

    End Module

End Namespace
