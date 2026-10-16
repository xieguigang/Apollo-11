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

            ' The executor switches on the value yaAGC calls ExtendedOpcode, which
            ' is "instruction >> 9".  This is the contract between the assembler and
            ' the CPU's Select Case, so print it for every instruction.
            Dim names As String() = New String() {"TC", "CCS", "TCF", "DAS", "LXCH", "INCR",
                                                  "ADS", "CA", "CS", "INDEX", "DXCH", "TS",
                                                  "XCH", "AD", "MASK", "READ", "WRITE", "RAND",
                                                  "WAND", "ROR", "WOR", "RXOR", "EDRUPT",
                                                  "BZF", "MSU", "DV", "QXCH", "AUG", "DIM",
                                                  "DCA", "DCS", "SU", "BZMF", "MP"}
            Console.WriteLine("CASEMAP")
            For Each nm As String In names
                Console.WriteLine("  " & nm.PadRight(7) & " case " &
                                  AgcOpcodeTable.DispatchValueForName(nm).ToString())
            Next
            Console.WriteLine("CASEMAP DLOAD word 077745 -> " &
                              AgcOpcodeTable.DispatchValueFromWord(&O77745).ToString())

            RunAritmeticProgram()
            RunInterpreterProgram()
            RunInterpretiveExecution()
            RunVectorTest()

            Console.WriteLine("Interpretive opcode table:")
            Console.WriteLine("  registered mnemonics: " & AgcOpcodeTable.InterpretiveCount.ToString())
            Dim collisions As List(Of String) = AgcOpcodeTable.InterpretiveCodeCollisions()
            Console.WriteLine("  opcodes shared by more than one mnemonic: " & collisions.Count.ToString())
            For Each c As String In collisions
                Console.WriteLine("    " & c)
            Next

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
        ''' Execute interpretive (Danzig) instructions on the virtual hardware.
        '''
        '''     SETLOC  04000
        '''     BANK
        '''         DLOAD   ONE         # MPAC = 1.0
        '''         DAD     TWO         # MPAC = 1.0 + 2.0 = 3.0
        '''         DCOMP               # MPAC = -3.0
        '''         DCOMP               # MPAC =  3.0
        '''         EXIT
        '''     ONE     2DEC    1.0
        '''     TWO     2DEC    2.0
        ''' <summary>
        ''' Execute a sequence of interpretive (Danzig) instructions on the virtual
        ''' hardware and check the arithmetic.
        '''
        '''     SETLOC  04000
        '''     BANK
        '''         DLOAD   ONE         # MPAC = 1
        '''         DAD     TWO         # MPAC = 3
        '''         DSU     ONE         # MPAC = 2
        '''         DCOMP               # MPAC = -2
        '''         DABS                # MPAC = 2
        '''         EXIT
        '''     ONE     2DEC    1.0
        '''     TWO     2DEC    2.0
        ''' </summary>
        Private Sub RunInterpretiveExecution()
            Console.WriteLine("Interpretive (Danzig) execution test:")

            Dim dir As String = Path.Combine(Path.GetTempPath(), "agc-danzig-" & Guid.NewGuid().ToString("N"))
            Directory.CreateDirectory(dir)
            Try
                Dim t As String = Chr(9) & Chr(9)
                Dim one As String = Chr(9)
                Dim src As New StringBuilder()
                src.Append(t & "SETLOC" & one & "4000" & vbCrLf)
                src.Append(t & "BANK" & vbCrLf)
                src.Append(one & "DLOAD" & t & "ONE" & vbCrLf)
                src.Append(one & "DAD" & t & "TWO" & vbCrLf)
                src.Append(one & "DSU" & t & "ONE" & vbCrLf)
                src.Append(one & "DCOMP" & vbCrLf)
                src.Append(one & "ABS" & vbCrLf)
                src.Append(one & "EXIT" & vbCrLf)
                src.Append("ONE" & t & "2DEC" & one & "1.0" & vbCrLf)
                src.Append("TWO" & t & "2DEC" & one & "2.0" & vbCrLf)
                File.WriteAllText(Path.Combine(dir, "MAIN.agc"), src.ToString())

                Dim asm As New AgcAssembler()
                asm.SourceDirectory = dir
                asm.MainFile = "MAIN.agc"
                Dim ok As Boolean = asm.Assemble()
                Check(ok, "interpretive program assembles", asm.Diagnostics.FormatAll(10))
                If Not ok Then Return

                ' The MPAC sign encoding is exercised on its own machine first so
                ' that it cannot disturb the instruction sequence below.
                Dim probe As New AgcMachine(asm.Image, asm.Symbols, asm.Diagnostics)
                probe.AttachConsole()
                probe.Reset()
                probe.Interpreter.SetMpacForTest(3)
                Dim rt1 As Integer = probe.Interpreter.Mpac
                probe.Interpreter.SetMpacForTest(-3)
                Dim rt2 As Integer = probe.Interpreter.Mpac
                Check(rt1 = 3, "MPAC round-trips +3", "got " & rt1.ToString())
                Check(rt2 = -3, "MPAC round-trips -3", "got " & rt2.ToString())

                Dim machine As New AgcMachine(asm.Image, asm.Symbols, asm.Diagnostics)
                machine.AttachConsole()
                machine.Reset()

                ' DLOAD, DAD and DSU are two basic words each (opcode plus operand);
                ' DCOMP and DABS are one word each because they take no operand.
                Dim afterDload As Integer
                Dim afterDad As Integer
                Dim afterDsu As Integer

                machine.StepOne()
                afterDload = machine.Interpreter.Mpac
                machine.StepOne()
                afterDad = machine.Interpreter.Mpac
                machine.StepOne()
                afterDsu = machine.Interpreter.Mpac
                machine.StepOne()                     ' DCOMP
                Dim afterDcomp As Integer = machine.Interpreter.Mpac
                machine.StepOne()                     ' DABS
                Dim afterDabs As Integer = machine.Interpreter.Mpac
                machine.StepOne()                     ' EXIT

                Console.WriteLine("  MPAC: DLOAD=" & afterDload.ToString() &
                                  " DAD=" & afterDad.ToString() &
                                  " DSU=" & afterDsu.ToString() &
                                  " DCOMP=" & afterDcomp.ToString() &
                                  " DABS=" & afterDabs.ToString() &
                                  " ; Z=" & AgcWord.ToOctal(machine.Cpu.Z))

                Check(afterDload = 1, "DLOAD 1.0 gives 1", afterDload.ToString())
                Check(afterDad = 3, "DAD 2.0 gives 3", afterDad.ToString())
                Check(afterDsu = 2, "DSU 1.0 gives 2", afterDsu.ToString())
                Check(afterDcomp = -2, "DCOMP gives -2", afterDcomp.ToString())
                Check(afterDabs = 2, "DABS gives 2", afterDabs.ToString())
                Check(machine.Interpreter.InstructionCount = 6,
                      "the interpreter executed six instructions",
                      machine.Interpreter.InstructionCount.ToString() & " executed")
                Check(Not machine.Cpu.Halted, "the CPU did not halt",
                      machine.Cpu.HaltReason)
            Finally
                Try
                    Directory.Delete(dir, True)
                Catch
                End Try
            End Try
        End Sub

        ''' <summary>
        ''' Exercise the vector and matrix path directly, without arranging an
        ''' assembled instruction sequence.
        ''' </summary>
        Private Sub RunVectorTest()
            Console.WriteLine("Vector and matrix operations:")

            Dim machine As AgcMachine = NewMachine()
            Dim interp As AgcInterpreter = machine.Interpreter

            ' A vector at 0120 holding (0.5, 0, 0).  Vector components are whole
            ' 15-bit words, so 0.5 is 020000 and zero is 0.  The addresses avoid
            ' the interpreter's push-down list, which occupies 0100..010F.
            machine.Memory.WriteErasable(&H120, &H2000)
            machine.Memory.WriteErasable(&H121, 0)
            machine.Memory.WriteErasable(&H122, 0)

            ' A matrix at 0130 whose first row is (1, 0, 0); 1.0 is 037777.
            machine.Memory.WriteErasable(&H130, &H7FFF)
            machine.Memory.WriteErasable(&H131, 0)
            machine.Memory.WriteErasable(&H132, 0)

            ' The matrix routines take the matrix address from the push-down list.
            interp.TestPushDown(&H130)
            Console.WriteLine("  matrix 0130: direct=" & machine.Memory.ReadErasable(&H130).ToString() &
                              " viaOperand=" & interp.TestReadComponent(&H130).ToString() &
                              " ; vector 0120: direct=" & machine.Memory.ReadErasable(&H120).ToString() &
                              " viaOperand=" & interp.TestReadComponent(&H120).ToString())

            Dim vec As Integer() = interp.TestReadVector(&H120)
            Console.WriteLine("  vector = (" & vec(0).ToString() & ", " & vec(1).ToString() &
                              ", " & vec(2).ToString() & ")")
            Check(vec(0) = 8192, "0.5 reads back as 8192 in a 15-bit component",
                  vec(0).ToString())

            interp.TestDotProduct(&H120)
            Dim dot As Integer = interp.Mpac
            Console.WriteLine("  dot product = " & dot.ToString())
            ' NOTE: the dot product currently returns 0 because the matrix operand
            ' read does not see the matrix even though PeekDown returns its address.
            ' The vector read is correct, so the fault is in the operand lookup for
            ' the matrix base rather than in the arithmetic.
            Check(dot = 8192, "matrix row (1,0,0) . vector (0.5,0,0) = 0.5", dot.ToString())
        End Sub

        ''' <summary>An empty but usable machine, for tests that need no rope.</summary>
        Private Function NewMachine() As AgcMachine
            Dim image As New AgcRopeImage()
            Dim symbols As New AgcSymbolTable()
            Dim diag As New AgcDiagnostics()
            Dim m As New AgcMachine(image, symbols, diag)
            m.AttachConsole()
            m.Reset()
            Return m
        End Function

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
                                             Console.WriteLine("    trace Z=" & AgcWord.ToOctal(c.Z) &
                                                               " instr=" & AgcWord.ToOctal(c.LastInstruction) &
                                                               " opcode=" & AgcWord.ToOctal(c.LastOpcode) &
                                                               " (dec instr=" & c.LastInstruction.ToString() &
                                                               " op=" & c.LastOpcode.ToString() & ")")
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
                asm._debugEncoding = True
                asm.Assemble()
                asm._debugEncoding = False

                ' The interpretive opcode is a word whose value is the complement of
                ' (code + 1); the operand word follows it.
                '
                ' DLOAD, DAD and the other double-precision instructions point at
                ' the LOW word of their argument, because the interpretive engine
                ' loads the low word and pairs it with the next lower address.  A
                ' "2DEC 1.0" at A therefore occupies A-1 (high) and A (low), and
                ' DLOAD A encodes A + 1, not A.
                Dim aPseudo As Integer = asm.Lookup("A")
                Dim bPseudo As Integer = asm.Lookup("B")
                Dim aAddr As AgcAddress = AgcCodec.PseudoToAddress(aPseudo)
                Dim bAddr As AgcAddress = AgcCodec.PseudoToAddress(bPseudo)
                Dim dloadOpcode As Integer = asm.Image.GetFixed(2, 2048)
                Dim dloadOperand As Integer = asm.Image.GetFixed(2, 2049)
                Dim dadOperand As Integer = asm.Image.GetFixed(2, 2051)
                Console.WriteLine("  A addr=" & aAddr.ToString() &
                                  " B addr=" & bAddr.ToString() &
                                  " DLOAD opcode=" & AgcWord.ToOctal(dloadOpcode) &
                                  " operand=" & AgcWord.ToOctal(dloadOperand) &
                                  " DAD operand=" & AgcWord.ToOctal(dadOperand))

                CheckEqual(dloadOperand, (aAddr.SReg + 1) And AgcWord.Mask15,
                           "DLOAD operand is the low word of A")
                CheckEqual(dadOperand, (bAddr.SReg + 1) And AgcWord.Mask15,
                           "DAD operand is the low word of B")
                Check(dloadOpcode <> 0, "DLOAD opcode word is non-zero")
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
