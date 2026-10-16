' ============================================================================
'  AgcInterpreter.vb -- the runtime execution engine for the AGC's
'  "interpretive" (Danzig) instruction set.
'
'  The AGC has two instruction sets.  The basic set is handled directly by
'  AgcCpu.  The interpretive set is a virtual machine layered on top: the CPU
'  sees a word that no basic opcode matches, and this class executes it.
'
'  State of the virtual machine, all held in ordinary AGC erasable memory so
'  that the flight software can save and restore it across an interrupt:
'
'      MPAC      the 14-bit working accumulator, at addresses 0 and 1
'      BUF       a push-down list of 16 words, addressed through the "PUSH"
'                and "PDDL"/"PDVL" style instructions
'      LOC       the interpretive program counter (a basic-memory address)
'      ADDRWD    the operand of the instruction being executed
'      PUSHLOC   the saved BUF pointer
'
'  Instructions are one or two basic words: the instruction word followed by
'  its operand.  A word whose value is the complement of (code + 1) selects an
'  interpretive instruction; see AssemblerInterpreter.vb for the encoding.
' ============================================================================

Imports System
Imports System.Collections.Generic

Namespace Agc

    Public Class AgcInterpreter

        ' --- Addresses of the interpretive machine's registers in erasable memory.
        Public Const AddrMpac As Integer = 0        ' MPAC occupies 0 and 1
        Public Const AddrMpacLow As Integer = 1
        Public Const AddrLoc As Integer = &H8       ' LOC, the interpretive PC
        Public Const AddrBuf As Integer = &H9       ' BUF, the push-down list
        Public Const AddrPushes As Integer = &HA     ' PUSHLOC is a register, see below
        Public Const AddrAddrwd As Integer = &HD    ' ADDRWD
        Public Const AddrPushtime As Integer = &HE

        ' --- Registers that live in the CPU itself rather than in erasable memory.
        ' LOC is a normal erasable word; the push-down pointer is also erasable.

        Private ReadOnly _cpu As AgcCpu
        Private ReadOnly _memory As AgcMemory

        ''' <summary>
        ''' The interpretive opcodes this engine executes, as "NAME=code" strings
        ''' with the code in octal.  Anything not listed will halt the CPU with an
        ''' "unrecognised opcode" diagnostic rather than silently doing nothing.
        ''' </summary>
        Public Shared Function ImplementedOpcodes() As List(Of String)
            Dim result As New List(Of String)()
            For Each item As String In New String() { _
                "EXIT=000", "VLOAD=001", "VLOAD*=003", "CALL=016", "DSQ=024", "INCR,2=026",
                "INCR,1=030", "DLOAD=031", "DLOAD*=033", "DCOMP=032", "SL=034", "SR=036",
                "DDV=037", "DDV*=039", "ABS=044", "VADD=045", "VSUB=047", "VXSC=049",
                "BZE=052", "GOTO=056", "ABS/BPL=058", "CLEAR=062", "POP=064",
                "PUSH=070", "DSU=071", "DSU*=073", "DAD=077", "DAD*=079", "DMP=081", "DMP*=083"}
                result.Add(item)
            Next
            Return result
        End Function

        ''' <summary>Number of interpretive instructions executed.</summary>
        Public Property InstructionCount As Integer

        ''' <summary>
        ''' Set MPAC directly.  Used by tests to exercise the sign encoding without
        ''' going through an instruction.
        ''' </summary>
        Public Sub SetMpacForTest(value As Integer)
            WriteMpac(value)
        End Sub

        ''' <summary>Push a word, for tests that need to prime the matrix pointer.</summary>
        Public Sub TestPushDown(value As Integer)
            PushDown(value)
        End Sub

        ''' <summary>Read a three-component vector, for tests.</summary>
        Public Function TestReadVector(baseAddr As Integer) As Integer()
            Return ReadVector(baseAddr)
        End Function

        ''' <summary>Read one component through the operand path, for tests.</summary>
        Public Function TestReadComponent(a As Integer) As Integer
            Return ReadComponent(a)
        End Function

        ''' <summary>Run a dot product, for tests.</summary>
        Public Sub TestDotProduct(operand As Integer)
            DotProduct(operand)
        End Sub

        ''' <summary>Run a cross product of MPAC with the vector at the operand.</summary>
        Public Sub TestCrossProduct(operand As Integer)
            CrossProduct(operand)
        End Sub

        ''' <summary>Run a vector projection of the operand onto MPAC.</summary>
        Public Sub TestVectorProjection(operand As Integer)
            VectorProjection(operand)
        End Sub

        ''' <summary>Read one MPAC vector component, for tests.</summary>
        Public Function TestMpacComponent(index As Integer) As Integer
            Return ReadComponent(AddrMpac + index)
        End Function

        ''' <summary>Read the raw erasable word behind a component, for tests.</summary>
        Public Function TestRawWord(a As Integer) As Integer
            Return _memory.ReadErasable(a And AgcWord.Mask12) And AgcWord.Mask15
        End Function

        ''' <summary>Development aid: print each interpretive instruction as it runs.</summary>
        Public Property TraceEnabled As Boolean

        ''' <summary>The interpretive program counter.  Held in erasable word 010.</summary>
        Public Property Loc As Integer

        ''' <summary>Index into the push-down list, 0..15.  Held in erasable word 011.</summary>
        Public Property PushPointer As Integer

        ''' <summary>The AGC erasable addresses of the 16 push-down words.</summary>
        Public ReadOnly PushList As Integer() = New Integer() { _
            &H100, &H101, &H102, &H103, &H104, &H105, &H106, &H107, _
            &H108, &H109, &H10A, &H10B, &H10C, &H10D, &H10E, &H10F}

        Public Sub New(cpu As AgcCpu, memory As AgcMemory)
            _cpu = cpu
            _memory = memory
        End Sub

        Public Sub Reset()
            InstructionCount = 0
            Loc = 0
            PushPointer = 0
        End Sub

        ' ------------------------------------------------------------------
        ' Memory access helpers
        ' ------------------------------------------------------------------

        ''' <summary>
        ''' Read interpretive machine state.
        '''
        ''' This deliberately goes to the erasable memory array rather than through
        ''' the CPU's register-aware Get/SetErasable.  In the real AGC, MPAC lives at
        ''' erasable 0 and 1, but addresses 0, 1 and 2 are also the A, L and Q
        ''' registers, so the register path would split MPAC between a register and
        ''' memory.  The interpretive engine sees MPAC as two plain memory words,
        ''' which is how the flight software reaches it.
        ''' </summary>
        Private Function ReadErasable(a As Integer) As Integer
            Return _memory.ReadErasable(a And AgcWord.Mask12) And AgcWord.Mask15
        End Function

        Private Sub WriteErasable(a As Integer, value As Integer)
            _memory.WriteErasable(a And AgcWord.Mask12, value And AgcWord.Mask15)
        End Sub

        ''' <summary>
        ''' Read an interpretive operand.  An interpretive operand word is a full
        ''' 15-bit AGC address: values below 04000 are erasable, everything above is
        ''' fixed and is decoded through the bank registers, which is why the value
        ''' must be passed to the memory unit intact.
        ''' </summary>
        Private Function ReadOperand(a As Integer) As Integer
            Dim addr As Integer = a And AgcWord.Mask15
            If addr < &H400 Then Return ReadErasable(addr)
            Return _memory.ReadFixed(addr) And AgcWord.Mask15
        End Function

        Private Sub WriteOperand(a As Integer, value As Integer)
            Dim addr As Integer = a And AgcWord.Mask15
            If addr < &H400 Then
                WriteErasable(addr, value)
            Else
                _memory.WriteFixed(addr, value)
            End If
        End Sub

        ''' <summary>Read a 14-bit MPAC-style value with sign extension.</summary>
        Private Function ReadSigned(a As Integer) As Integer
            Dim v As Integer = ReadErasable(a)
            If (v And &H4000) <> 0 Then
                ' A negative value: complement the magnitude.
                Return -(((AgcWord.Mask15 - v) + 1) And AgcWord.Mask15)
            End If
            Return v
        End Function

        ''' <summary>Store a signed value in AGC one's-complement 15-bit form.</summary>
        Private Function EncodeSigned(v As Integer) As Integer
            If v >= 0 Then Return v And AgcWord.Mask15
            Return (AgcWord.Mask15 - ((-v) - 1)) And AgcWord.Mask15
        End Function

        ''' <summary>Modulus of a single MPAC word: 14 bits.</summary>
        Private Const MpacModulus As Integer = &H4000

        ''' <summary>Mask of a single 14-bit MPAC word.</summary>
        Private Const MpacWordMask As Integer = &H3FFF

        ''' <summary>
        ''' Read MPAC as a signed integer.  MPAC is a 28-bit one's-complement
        ''' accumulator: a negative value is stored as the 28-bit complement of its
        ''' magnitude, so the sign bits occupy the top bits of the high word.  They
        ''' must be tested before the word is narrowed to 14 bits.
        ''' </summary>
        Private Function ReadMpac() As Integer
            Dim rawHi As Integer = ReadErasable(AddrMpac)
            Dim rawLo As Integer = ReadErasable(AddrMpacLow)
            If (rawHi And &H2000) <> 0 Then
                ' Complement each word within 14 bits to recover the magnitude.
                Dim cHi As Integer = (MpacWordMask Xor rawHi) And MpacWordMask
                Dim cLo As Integer = (MpacWordMask Xor rawLo) And MpacWordMask
                Return -((cHi << 14) Or cLo)
            End If
            Return ((rawHi And MpacWordMask) << 14) Or (rawLo And MpacWordMask)
        End Function

        ''' <summary>
        ''' MPAC as a signed value.  The raw words are a 14-bit one's-complement
        ''' pair, so callers should read the accumulator through here rather than
        ''' combining the words themselves.
        ''' </summary>
        Public ReadOnly Property Mpac As Integer
            Get
                Return ReadMpac()
            End Get
        End Property

        ''' <summary>
        ''' Store a signed integer into MPAC.  A negative value is the 28-bit
        ''' one's-complement of its magnitude, so each word is complemented within
        ''' 14 bits; the sign bits then live in the top bits of the high word.
        ''' </summary>
        Private Sub WriteMpac(value As Integer)
            If TraceEnabled Then
                Console.WriteLine("      WriteMpac(" & value.ToString() & ")")
            End If
            Dim mag As Long = Math.Abs(CLng(value))
            Dim hi As Integer = CInt(mag >> 14) And MpacWordMask
            Dim lo As Integer = CInt(mag And MpacWordMask)
            If value < 0 Then
                hi = (MpacWordMask Xor hi) And MpacWordMask
                lo = (MpacWordMask Xor lo) And MpacWordMask
            End If
            If TraceEnabled Then
                Console.WriteLine("      -> hi=" & AgcWord.ToOctal(hi) & " (" & hi.ToString() &
                                  ") lo=" & AgcWord.ToOctal(lo) & " (" & lo.ToString() & ")")
            End If
            WriteErasable(AddrMpac, hi)
            WriteErasable(AddrMpacLow, lo)
        End Sub

        ' ------------------------------------------------------------------
        ' Push-down list
        ' ------------------------------------------------------------------

        Public Sub PushDown(value As Integer)
            WriteErasable(PushList(PushPointer), value)
            PushPointer = (PushPointer + 1) And &HF
        End Sub

        Public Function PopUp() As Integer
            PushPointer = (PushPointer - 1) And &HF
            Return ReadErasable(PushList(PushPointer))
        End Function

        ''' <summary>Copy the top <paramref name="count"/> entries without popping.</summary>
        Private Function PeekDown(count As Integer) As Integer
            Dim idx As Integer = (PushPointer - count) And &HF
            Return ReadErasable(PushList(idx))
        End Function

        ' ------------------------------------------------------------------
        ' Execution
        ' ------------------------------------------------------------------

        ''' <summary>
        ''' Execute the interpretive instruction selected by <paramref name="word"/>.
        ''' Returns True when the instruction was understood.
        ''' </summary>
        ''' <summary>
        ''' Number of basic words the instruction just executed occupied.  Most
        ''' interpretive instructions are an opcode word followed by an operand word,
        ''' but one-operand-free forms such as DCOMP, DSQ, EXIT, PUSH and DDOUBL are a
        ''' single word.  The caller must advance the program counter by exactly this
        ''' many words or it will step over the next instruction.
        ''' </summary>
        Public Property StoreDebug As Boolean = False

        Public Property WordsConsumed As Integer

        ''' <summary>
        ''' Execute the interpretive instruction selected by <paramref name="word"/>.
        ''' Returns True when the instruction was understood; examine
        ''' <see cref="WordsConsumed"/> for how far the program counter should move.
        ''' </summary>
        Public Function Execute(word As Integer, operand As Integer) As Boolean
            InstructionCount += 1
            WordsConsumed = 2

            Dim halves As Integer = AgcWord.Mask15 Xor (word And AgcWord.Mask15)
            Dim code1 As Integer = (halves And &H7F) - 1
            Dim code2 As Integer = ((halves >> 7) And &H7F) - 1

            Dim code As Integer = -1
            If code2 >= 0 AndAlso AgcOpcodeTable.InterpretiveCodeExists(code2) Then
                ' A second opcode is present and is a real instruction; in the
                ' combined forms it is the one that executes.
                code = code2
            ElseIf code1 >= 0 AndAlso AgcOpcodeTable.InterpretiveCodeExists(code1) Then
                code = code1
            ElseIf code1 >= 0 Then
                code = code1
            ElseIf code2 >= 0 Then
                code = code2
            End If

            If TraceEnabled Then
                Console.WriteLine("    INTERP word=" & AgcWord.ToOctal(word) &
                                  " operand=" & AgcWord.ToOctal(operand) &
                                  " code1=" & code1.ToString() & " code2=" & code2.ToString() &
                                  " -> code=" & code.ToString())
            End If

            If code < 0 Then Return False

            If StoreDebug Then
                Console.WriteLine("      EXEC code=" & code.ToString() &
                                  " operand=0o" & Convert.ToString(operand, 8) &
                                  " code1=" & code1.ToString() & " code2=" & code2.ToString() &
                                  " word=0o" & Convert.ToString(word, 8))
            End If

            Dim ok As Boolean = ExecuteCode(code, operand)
            If ok Then
                If code = 0 Then
                    ' Code 0 is shared by EXIT and the whole STORE family, so
                    ' InterpretiveTakesOperand cannot answer for it: whichever of
                    ' EXIT and STORE is found first decides, and EXIT takes no
                    ' operand while a store does.  Getting this wrong mis-advances
                    ' the sequence by a word, which makes the interpretive program
                    ' walk into its own operands.  A store's operand word is
                    ' non-zero (it is a complemented store code); EXIT's is zero.
                    WordsConsumed = If((operand And AgcWord.Mask15) <> 0, 2, 1)
                Else
                    WordsConsumed = If(AgcOpcodeTable.InterpretiveTakesOperand(code), 2, 1)
                End If
            End If
            Return ok
        End Function

        Private Function ExecuteCode(code As Integer, operand As Integer) As Boolean
            Select Case code
                Case &H0
                    ' Opcode 0 is shared by EXIT and the whole STORE family.  The
                    ' interpreter tells them apart the same way the hardware does:
                    ' a store code is stored complemented so that it looks like a
                    ' negative word, while EXIT's operand word is zero.
                    ' (INTERPRETER.agc, "the storecode was stored complemented to
                    ' make it look like an opcode pair".)
                    Dim raw As Integer = operand And AgcWord.Mask15
                    ' A store code is a word the basic assembler would not use as
                    ' an ordinary address; the engine distinguishes it by being
                    ' non-zero, since EXIT's operand word is zero.
                    If raw <> 0 Then Return ExecuteStore(raw)
                    ' EXIT.  The interpretive sequence is finished, so the basic
                    ' program continues at the word after this instruction.  That is
                    ' already where Z points, so nothing has to be moved.
                    Loc = _cpu.Z And &HFFF
                    Return True

                Case &H19, &H1B                   ' DLOAD / DLOAD*
                    WriteMpac(ReadDouble(operand))
                    Return True

                Case &H71, &H73                   ' DAD / DAD*
                    Return DoubleAdd(operand, 1)

                Case &H69, &H6B                   ' DSU / DSU*
                    Return DoubleAdd(operand, -1)

                Case &H79, &H7B                   ' DMP / DMP*
                    Return DoubleMultiply(operand)

                Case &H45, &H47                   ' DDV / DDV*: double divide
                    Return DoubleDivide(operand)

                Case &H1, &H3                     ' VLOAD / VLOAD*
                    WriteMpac(CLng(ReadSignedOperand(operand)) << 14)
                    Return True

                Case &H49                        ' VADD
                    Return VectorAdd(operand, 1)

                Case &H4B                        ' VSUB
                    Return VectorAdd(operand, -1)

                Case &H4D                        ' VXSC: MPAC = MPAC * scalar
                    WriteMpac((ReadMpac() * ReadSignedOperand(operand)) >> 14)
                    Return True

                Case &H40                        ' DCOMP: MPAC = -MPAC
                    WriteMpac(-ReadMpac())
                    Return True

                Case &H58                        ' ABS: MPAC = |MPAC|
                    Dim v As Integer = ReadMpac()
                    If v < 0 Then WriteMpac(-v)
                    Return True

                Case &H30                        ' DSQ: MPAC = MPAC squared
                    Dim sq As Integer = ReadMpac()
                    WriteMpac((CLng(sq) * CLng(sq)) >> 28)
                    Return True

                Case &H36                        ' INCR,1
                    WriteMpac(ReadMpac() + (&H4000L << 14))
                    Return True

                Case &H32                        ' INCR,2
                    WriteMpac(ReadMpac() + &H4000L)
                    Return True

                Case &H78                        ' PUSH: save MPAC's high word
                    PushDown(ReadErasable(AddrMpac))
                    Return True

                Case &H76                        ' POP: restore MPAC's high word
                    WriteErasable(AddrMpac, PopUp())
                    Return True

                Case &H18                        ' CLEAR / CLR: MPAC = 0
                    WriteMpac(0)
                    Return True

                Case &H46, &H42                   ' SL / SR shifts by the operand count
                    Return ShiftMpac(operand, code = &H46)

                Case &H56                        ' GOTO
                    Return GoToAddress(operand)

                Case &H16                        ' CALL / CALRB
                    PushDown((_cpu.Z + 1) And &HFFF)
                    Return GoToAddress(operand)

                Case &H2D, &H2F                   ' MXV / MXV*: MPAC = M * V
                    Return MatrixTimesVector(operand)

                Case &H5D, &H5F                   ' DOT / DOT*: MPAC = M . V
                    Return DotProduct(operand)

                Case &H5A                        ' BPL: branch when MPAC >= 0
                    If ReadMpac() >= 0 Then Return GoToAddress(operand)
                    Return True

                Case &H5E                        ' BMN: branch when MPAC < 0
                    If ReadMpac() < 0 Then Return GoToAddress(operand)
                    Return True

                Case &H61                        ' VXV: MPAC = MPAC x V (cross product)
                    Return CrossProduct(operand)

                Case &H65                        ' VPROJ: MPAC = V . MPAC / |MPAC|
                    Return VectorProjection(operand)

                Case &H29, &H2B                   ' PDDL / PDDL*: push down the operand
                    PushDown(ReadOperand(operand))
                    Return True

                Case &H31, &H33                   ' PDVL / PDVL*: push the vector's first word
                    PushDown(ReadOperand(operand))
                    Return True

                Case &H3D, &H3F                   ' NORM / NORM*: normalise MPAC
                    Return NormaliseMpac(operand)
                Case &H52                        ' BZE: branch when MPAC = 0
                    If ReadMpac() = 0 Then Return GoToAddress(operand)
                    Return True

                Case Else
                    Return False
            End Select
        End Function

        ''' <summary>
        ''' A branch inside the interpretive program.  The whole interpretive
        ''' instruction is replaced, so the target becomes the new basic program
        ''' counter and execution continues there.
        ''' </summary>
        Private Function GoToAddress(target As Integer) As Boolean
            Dim addr As Integer = target And AgcWord.Mask15
            ' The target is either erasable (below 04000) or fixed; both are
            ' expressed as a basic-memory address.
            _cpu.Z = addr
            Loc = addr
            Return True
        End Function

        ''' <summary>Shift MPAC left or right, filling with zeros.</summary>
        Private Function ShiftMpac(count As Integer, left As Boolean) As Boolean
            Dim n As Integer = count And &HF
            Dim v As Integer = ReadMpac()
            If left Then
                WriteMpac(v << n)
            Else
                WriteMpac(v >> n)
            End If
            Return True
        End Function

        ' ------------------------------------------------------------------
        ' Double-precision arithmetic
        ' ------------------------------------------------------------------

        ''' <summary>
        ''' Read the 28-bit double-precision value whose LOW word is at
        ''' <paramref name="operand"/>.  DLOAD, DAD and DSU address the low word, so
        ''' the high word is one address below.
        ''' </summary>
        Private Function ReadDouble(operand As Integer) As Integer
            Dim hi As Integer = ReadOperand(operand - 1) And MpacWordMask
            Dim lo As Integer = ReadOperand(operand) And MpacWordMask
            If (hi And &H2000) <> 0 Then
                Dim cHi As Integer = (MpacWordMask Xor hi) And MpacWordMask
                Dim cLo As Integer = (MpacWordMask Xor lo) And MpacWordMask
                Return -((cHi << 14) Or cLo)
            End If
            Return (hi << 14) Or lo
        End Function

        ''' <summary>Read a 15-bit scalar operand as a signed value.</summary>
        Private Function ReadSignedOperand(a As Integer) As Integer
            Dim v As Integer = ReadOperand(a)
            If (v And &H4000) <> 0 Then
                Return -((MpacWordMask Xor v) And MpacWordMask)
            End If
            Return v And MpacWordMask
        End Function

        ''' <summary>MPAC = MPAC +/- the 28-bit value at the operand.</summary>
        Private Function DoubleAdd(operand As Integer, sign As Integer) As Boolean
            Dim other As Integer = ReadDouble(operand)
            WriteMpac(ReadMpac() + sign * other)
            Return True
        End Function

        ''' <summary>MPAC = MPAC * the 28-bit value at the operand.</summary>
        Private Function DoubleMultiply(operand As Integer) As Boolean
            Dim product As Long = CLng(ReadMpac()) * CLng(ReadDouble(operand))
            WriteMpac(CInt(product >> 28))
            Return True
        End Function

        ''' <summary>MPAC = MPAC / the 28-bit value at the operand.</summary>
        Private Function DoubleDivide(operand As Integer) As Boolean
            Dim divisor As Integer = ReadDouble(operand)
            If divisor = 0 Then Return True          ' divide by zero is a no-op here
            WriteMpac(CInt((CLng(ReadMpac()) << 28) \ CLng(divisor)))
            Return True
        End Function

        ''' <summary>MPAC = MPAC +/- the 15-bit scalar at the operand.</summary>
        Private Function VectorAdd(operand As Integer, sign As Integer) As Boolean
            Dim v As Integer = ReadSignedOperand(operand)
            WriteMpac(ReadMpac() + CLng(sign * v) * &H4000L)
            Return True
        End Function

        ' ------------------------------------------------------------------
        ' Vectors and matrices
        '
        ' A vector is three consecutive words, each a 15-bit component.  A matrix
        ' is nine consecutive words read as three rows.  The matrix routines take
        ' the matrix address from the push-down list (the usual "VXSC / MXV"
        ' idiom pushes it first with SETPD or PDDL) and the vector address from
        ' the instruction operand.
        ' ------------------------------------------------------------------

        Private Const VectorComponents As Integer = 3

        ''' <summary>
        ''' Read a 15-bit vector component as a signed integer.  Vector and matrix
        ''' components are whole 15-bit words, unlike MPAC's 14-bit halves, so 1.0
        ''' is 037777 and 0.5 is 020000.
        ''' </summary>
        Private Function ReadComponent(a As Integer) As Integer
            Dim v As Integer = ReadOperand(a) And AgcWord.Mask15
            If (v And &H4000) <> 0 Then
                Return -((AgcWord.Mask15 Xor v) And AgcWord.Mask15)
            End If
            Return v
        End Function

        Private Function ReadVector(baseAddr As Integer) As Integer()
            Dim v(VectorComponents - 1) As Integer
            For i As Integer = 0 To VectorComponents - 1
                v(i) = ReadComponent(baseAddr + i)
            Next
            Return v
        End Function

        ''' <summary>
        ''' Execute a STORE-family instruction.  The operand word is a store code:
        ''' the assembler stores the complement of (address + 1), so a negative word
        ''' means a store.  Bits B12-B14 select the variant and the low eleven bits
        ''' are the erasable destination, matching the hardware, which masks with
        ''' B12T14 for the code number and LOW11 for the address.
        '''
        ''' The variants are STORE, STORE,1, STORE,2, STODL, STODL,1, STOVL,
        ''' STOVL,1 and STCALL.  The double-precision stores save MPAC and then
        ''' replace it from the following word.
        ''' </summary>
        Private Function ExecuteStore(storeWord As Integer) As Boolean
            Dim value As Integer = ((AgcWord.Mask15 Xor storeWord) And MpacWordMask) - 1
            Dim storeNumber As Integer = (value >> 11) And 7
            Dim address As Integer = value And &H7FF
            If TraceEnabled Then
                Console.WriteLine("      STORE no=" & storeNumber.ToString() &
                                  " addr=" & AgcWord.ToOctal(address) &
                                  " (word=" & AgcWord.ToOctal(storeWord) & ")")
            End If

            Dim scaled As Boolean = (storeNumber And 1) = 1

            Select Case storeNumber
                Case 0, 1, 2
                    ' STORE, STORE,1, STORE,2: save MPAC and carry on in the
                    ' interpretive section.  The ,1 and ,2 forms scale by the
                    ' combined index registers, which the engine does not model.
                    WriteMpacTo(address)
                    Return True

                Case 3, 4
                    ' STODL, STODL,1: store, then reload MPAC in double precision
                    ' from the address in the following word.
                    WriteMpacTo(address)
                    Dim reload As Integer = ReadOperand(PeekNextOperandWord())
                    WriteMpac(reload)
                    Return True

                Case 5, 6
                    ' STOVL, STOVL,1: store, then reload a vector from the address
                    ' in the following word.
                    WriteMpacTo(address)
                    Dim vecAddr As Integer = PeekNextOperandWord()
                    Dim v As Integer() = ReadVector(vecAddr)
                    WriteMpacVector(v)
                    Return True

                Case 7
                    ' STCALL: store MPAC.  QPRET is not modelled yet, so the call
                    ' itself is not performed; the store still happens, which is the
                    ' observable effect the flight software depends on.
                    WriteMpacTo(address)
                    Return True
            End Select

            Return True
        End Function

        ''' <summary>
        ''' Write the 28-bit MPAC into two consecutive erasable words.  MPAC is a
        ''' one's-complement pair, so each 14-bit half is written independently,
        ''' which is what the hardware does.
        ''' </summary>
        Private Sub WriteMpacTo(address As Integer)
            Dim rawHi As Integer = ReadErasable(AddrMpac)
            Dim rawLo As Integer = ReadErasable(AddrMpacLow)
            WriteOperand(address, rawHi)
            WriteOperand(address + 1, rawLo)
        End Sub

        ''' <summary>
        ''' The operand word that follows the current instruction, as the hardware
        ''' would see it.  STODL and STOVL take their reload address from here.
        ''' </summary>
        Private Function PeekNextOperandWord() As Integer
            Dim at As Integer = (_cpu.Z + 1) And AgcWord.Mask12
            Return ReadErasable(at)
        End Function

        ''' <summary>
        ''' MPAC = MPAC x V, the cross product.  Components are fractions, so each
        ''' product is divided by 040000.  The result is built word by word because
        ''' the left operand is the vector already in MPAC.
        ''' </summary>
        Private Function CrossProduct(operand As Integer) As Boolean
            Dim a As Integer() = ReadVector(AddrMpac)
            Dim b As Integer() = ReadVector(operand)
            Dim result(VectorComponents - 1) As Integer
            ' x = a1*b2 - a2*b1, y = a2*b0 - a0*b2, z = a0*b1 - a1*b0
            result(0) = CInt((CLng(a(1)) * CLng(b(2)) - CLng(a(2)) * CLng(b(1))) >> 14)
            result(1) = CInt((CLng(a(2)) * CLng(b(0)) - CLng(a(0)) * CLng(b(2))) >> 14)
            result(2) = CInt((CLng(a(0)) * CLng(b(1)) - CLng(a(1)) * CLng(b(0))) >> 14)
            WriteMpacVector(result)
            Return True
        End Function

        ''' <summary>
        ''' VPROJ: MPAC = (V . MPAC / |MPAC|) * (MPAC / |MPAC|).  The vector in the
        ''' operand is projected onto the unit vector in the direction of MPAC.
        ''' </summary>
        Private Function VectorProjection(operand As Integer) As Boolean
            Dim u As Integer() = ReadVector(AddrMpac)
            Dim v As Integer() = ReadVector(operand)

            Dim magSq As Long = 0
            For i As Integer = 0 To VectorComponents - 1
                magSq += CLng(u(i)) * CLng(u(i))
            Next
            If magSq = 0 Then Return True

            ' |MPAC|, as a fraction comparable with the components.
            Dim mag As Long = CLng(Math.Sqrt(CDbl(magSq)))
            If mag = 0 Then Return True

            Dim dot As Long = 0
            For i As Integer = 0 To VectorComponents - 1
                dot += CLng(v(i)) * CLng(u(i))
            Next

            ' scale = (V . u) / |MPAC|^2, so result = scale * u reproduces the
            ' projection without ever forming the unit vector explicitly.
            Dim result(VectorComponents - 1) As Integer
            For i As Integer = 0 To VectorComponents - 1
                result(i) = CInt((dot * CLng(u(i))) \ magSq)
            Next
            WriteMpacVector(result)
            Return True
        End Function

        ''' <summary>Write three 15-bit components into MPAC, most significant first.</summary>
        Private Sub WriteMpacVector(components As Integer())
            WriteMpac(0)
            For i As Integer = 0 To VectorComponents - 1
                Dim w As Integer = components(i) And AgcWord.Mask15
                WriteOperand(AddrMpac + i, w)
            Next
        End Sub

        ''' <summary>
        ''' MPAC = the matrix at the top of the push-down list times the vector at
        ''' the operand.  A component product is divided by 040000 because the
        ''' components are fractions.
        ''' </summary>
        Private Function MatrixTimesVector(operand As Integer) As Boolean
            Dim matrixAddr As Integer = PeekDown(1)
            Dim vec As Integer() = ReadVector(operand)
            Dim result(VectorComponents - 1) As Integer
            For row As Integer = 0 To VectorComponents - 1
                Dim sum As Long = 0
                For col As Integer = 0 To VectorComponents - 1
                    sum += CLng(ReadComponent(matrixAddr + row * VectorComponents + col)) * CLng(vec(col))
                Next
                result(row) = CInt(sum >> 14)
            Next
            ' Only the first two components fit in MPAC, which is the AGC's
            ' convention: the third is left for the caller to handle.
            WriteMpac((CLng(result(0)) << 14) Or (result(1) And MpacWordMask))
            Return True
        End Function

        ''' <summary>MPAC = the dot product of the matrix's first row and the vector.</summary>
        Private Function DotProduct(operand As Integer) As Boolean
            Dim matrixAddr As Integer = PeekDown(1)
            Dim vec As Integer() = ReadVector(operand)
            Dim sum As Long = 0
            For col As Integer = 0 To VectorComponents - 1
                sum += CLng(ReadComponent(matrixAddr + col)) * CLng(vec(col))
            Next
            WriteMpac(CInt(sum >> 14))
            Return True
        End Function

        ''' <summary>
        ''' Normalise MPAC: shift it left until its most significant bit is set,
        ''' leaving the accumulated shift count at the operand address.  The AGC
        ''' uses this to bring a vector into a known range before a divide.
        ''' </summary>
        Private Function NormaliseMpac(operand As Integer) As Boolean
            Dim v As Integer = ReadMpac()
            Dim count As Integer = 0
            If v <> 0 Then
                Dim mag As Long = Math.Abs(CLng(v))
                While (mag And &H10000000L) = 0 AndAlso count < 28
                    mag <<= 1
                    count += 1
                End While
                If v < 0 Then mag = -mag
                v = CInt(mag)
            End If
            WriteMpac(v)
            WriteOperand(operand, count)
            Return True
        End Function

    End Class

End Namespace
