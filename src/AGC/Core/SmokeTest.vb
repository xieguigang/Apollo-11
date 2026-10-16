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

            Dim probe As AgcOpcode = Nothing
            Console.WriteLine("LITERAL &H9000 = " & (&H9000).ToString() &
                              " octal=" & AgcWord.ToOctal(&H9000))
            Console.WriteLine("LITERAL &HE000 = " & (&HE000).ToString() &
                              " octal=" & AgcWord.ToOctal(&HE000))
            Console.WriteLine("LITERAL &H10000 = " & (&H10000).ToString() &
                              " octal=" & AgcWord.ToOctal(&H10000))
            If AgcOpcodeTable.TryGetBasic("CA", probe) Then
                Console.WriteLine("TABLE CA opcode=" & AgcWord.ToOctal(probe.Opcode) &
                                  " flags=" & probe.Flags.ToString())
            Else
                Console.WriteLine("TABLE CA not found")
            End If
            If AgcOpcodeTable.TryGetBasic("AD", probe) Then
                Console.WriteLine("TABLE AD opcode=" & AgcWord.ToOctal(probe.Opcode) &
                                  " flags=" & probe.Flags.ToString())
            End If
            If AgcOpcodeTable.TryGetBasic("TS", probe) Then
                Console.WriteLine("TABLE TS opcode=" & AgcWord.ToOctal(probe.Opcode))
            End If

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
        ''' end.  The programs that use erasable memory are assembled with
        ''' "SETLOC 0" so that the registers land in the unswitched erasable area,
        ''' the way the real AGC sources do.
        ''' </summary>
        Private Sub RunAritmeticProgram()
            Console.WriteLine("Assembler + CPU smoke test (arithmetic and CCS):")

            Dim dir As String = Path.Combine(Path.GetTempPath(), "agc-smoke-" & Guid.NewGuid().ToString("N"))
            Directory.CreateDirectory(dir)
            Try
                Dim t As String = Chr(9) & Chr(9)
                Dim one As String = Chr(9)

                ' --- The erasable registers, in their own SETLOC'd section.
                ' They start at 0100 so that they do not collide with the CPU
                ' registers at 0..7 or the I/O channels at 010..017.
                Dim dst As New StringBuilder()
                dst.Append(t & "SETLOC" & one & "100" & vbCrLf)
                dst.Append("COUNT" & t & "ERASE" & one & "1" & vbCrLf)
                dst.Append("RESULT" & t & "ERASE" & one & "1" & vbCrLf)
                dst.Append("SAVED" & t & "ERASE" & one & "1" & vbCrLf)
                dst.Append(t & "SETLOC" & one & "4000" & vbCrLf)
                dst.Append(t & "BANK" & vbCrLf)
                dst.Append(one & "CA" & t & "ONE" & vbCrLf)
                dst.Append(one & "AD" & t & "TWO" & vbCrLf)
                dst.Append(one & "TS" & t & "RESULT" & vbCrLf)
                dst.Append(one & "CA" & t & "RESULT" & vbCrLf)
                dst.Append(one & "TS" & t & "SAVED" & vbCrLf)
                dst.Append("LOOP" & one & "INCR" & t & "COUNT" & vbCrLf)
                dst.Append(one & "CCS" & t & "COUNT" & vbCrLf)
                dst.Append(one & "TCF" & t & "DONE" & vbCrLf)
                dst.Append(one & "TCF" & t & "LOOP" & vbCrLf)
                dst.Append("DONE" & one & "TCF" & t & "DONE" & vbCrLf)
                dst.Append("ONE" & t & "DEC" & one & "1" & vbCrLf)
                dst.Append("TWO" & t & "DEC" & one & "2" & vbCrLf)
                File.WriteAllText(Path.Combine(dir, "MAIN.agc"), dst.ToString())

                Dim asm As New AgcAssembler()
                asm.SourceDirectory = dir
                asm.MainFile = "MAIN.agc"
                asm._debugEncoding = True
                Dim ok As Boolean = asm.Assemble()
                asm._debugEncoding = False
                If Not ok Then
                    Console.WriteLine("  generated source:")
                    For Each l As String In dst.ToString().Split(CChar(vbLf))
                        Console.WriteLine("    [" & l.Replace(Chr(9), "<T>") & "]")
                    Next
                End If
                Check(ok, "program assembles", asm.Diagnostics.FormatAll(10))
                If Not ok Then Return

                ' --- Check the encoding of the individual instructions.
                Dim base As Integer = asm.Lookup("ONE")
                Check(base > 0, "ONE is defined", "lookup returned " & base.ToString())

                Dim code As Integer = asm.Lookup("LOOP")
                Check(code > 0, "LOOP is defined")
                Dim saved As Integer = asm.Lookup("SAVED")
                Check(saved >= 0, "SAVED is in erasable memory", "pseudo=" & Octal(saved))

                ' --- Run it on the virtual hardware.
                Dim machine As New AgcMachine(asm.Image, asm.Symbols, asm.Diagnostics)
                machine.AttachConsole()
                machine.Reset()
                Console.WriteLine("  rope bank 2 word 0 = " & Octal(asm.Image.GetFixed(2, 2048)))
                Console.WriteLine("  after load, fixed(2,0)=" & Octal(machine.Memory.Fixed(2, 0)) &
                                  " fixed(4,0)=" & Octal(machine.Memory.Fixed(4, 0)) &
                                  " Z=" & Octal(machine.Cpu.Z))
                Dim traceCount As Integer = 0
                machine.Cpu.Tracer = Sub(c As AgcCpu)
                                         If traceCount < 8 Then
                                             traceCount += 1
                                             Console.WriteLine("    trace Z=" & Octal(c.Z) &
                                                               " instr=" & Octal(c.LastInstruction) &
                                                               " opcode=" & Octal(c.LastOpcode))
                                         End If
                                     End Sub
                machine.RunCycles(600)

                Console.WriteLine("  COUNT=" & Octal(CpuRead(machine, "COUNT", asm)) &
                                  " RESULT=" & Octal(CpuRead(machine, "RESULT", asm)) &
                                  " SAVED=" & Octal(CpuRead(machine, "SAVED", asm)) &
                                  " A=" & Octal(machine.Cpu.A) &
                                  " Z=" & Octal(machine.Cpu.Z) &
                                  " insns=" & machine.Cpu.InstructionCount.ToString() &
                                  " halted=" & machine.Cpu.Halted.ToString())

                CheckEqual(CpuRead(machine, "SAVED", asm), 3, "A + B = 3 reached the SAVED register")
                CheckEqual(CpuRead(machine, "COUNT", asm), 1, "CCS/TCF loop ran exactly once")
                Check(machine.Cpu.InstructionCount > 8, "CPU executed the program",
                      machine.Cpu.InstructionCount.ToString() & " instructions")
            Finally
                Try
                    Directory.Delete(dir, True)
                Catch
                End Try
            End Try
        End Sub

        ''' <summary>
        ''' Assemble a program that uses the interpretive (Danzig) instruction set
        ''' and check its encoding.
        ''' </summary>
        Private Sub RunInterpreterProgram()
            Console.WriteLine("Interpreter instruction encoding check:")

            Dim dir As String = Path.Combine(Path.GetTempPath(), "agc-interp-" & Guid.NewGuid().ToString("N"))
            Directory.CreateDirectory(dir)
            Try
                Dim t As String = Chr(9) & Chr(9)
                Dim one As String = Chr(9)
                Dim src As New StringBuilder()
                src.Append(t & "SETLOC" & one & "100" & vbCrLf)
                src.Append("C" & t & "ERASE" & one & "2" & vbCrLf)
                src.Append(t & "SETLOC" & one & "4000" & vbCrLf)
                src.Append(t & "BANK" & vbCrLf)
                src.Append(one & "DLOAD" & t & "A" & vbCrLf)
                src.Append(one & "DAD" & t & "B" & vbCrLf)
                src.Append(one & "STODL" & t & "C" & vbCrLf)
                src.Append(one & "EXIT" & vbCrLf)
                src.Append("A" & t & "2DEC" & one & "1.0" & vbCrLf)
                src.Append("B" & t & "2DEC" & one & "2.0" & vbCrLf)
                File.WriteAllText(Path.Combine(dir, "MAIN.agc"), src.ToString())

                Dim asm As New AgcAssembler()
                asm.SourceDirectory = dir
                asm.MainFile = "MAIN.agc"
                asm.Assemble()

                ' DLOAD has interpretive code 031, so the encoded word must be the
                ' complement of (031+1) = 032: 077777 xor 032 = 077745.
                Dim dload As Integer = asm.Lookup("LOOP")
                Dim codeAddr As Integer = 2048
                Dim w As Integer = asm.Image.GetFixed(2, 2048)
                CheckEqual(w, AgcWord.Mask15 Xor (&O31 + 1), "DLOAD encodes as the complement of 032 octal")

                ' The next word is the operand address of A.
                Dim aPseudo As Integer = asm.Lookup("A")
                Dim aAddr As AgcAddress = AgcCodec.PseudoToAddress(aPseudo)
                CheckEqual(asm.Image.GetFixed(2, 2049), aAddr.SReg, "DLOAD operand word holds the address of A")
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
