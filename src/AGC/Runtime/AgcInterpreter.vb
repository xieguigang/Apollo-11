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
        ''' The interpretive opcodes this engine executes, as a sorted list of
        ''' "NAME=code" strings.  Anything not listed will halt the CPU with an
        ''' "unrecognised opcode" diagnostic rather than silently doing nothing.
        ''' </summary>
        Public Shared Function ImplementedOpcodes() As List(Of String)
            Dim result As New List(Of String)()
            For Each item As String In New String() { _
                "EXIT=000", "VLOAD=001", "VLOAD*=003", "CALL=016", "DLOAD=031", "DLOAD*=033",
                "DSQ=030", "INCR,2=032", "INCR,1=036", "DCOMP=040", "SR=042", "ABS=058",
                "DDV=045", "DDV*=047", "SL=046", "VADD=049", "VSUB=051", "VXSC=055",
                "PUSH=078", "POP=076", "CLEAR=072", "BZE=052", "BPL=058", "GOTO=056",
                "DSU=151", "DSU*=153", "DAD=161", "DAD*=163", "DMP=171", "DMP*=173"}
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

            Dim ok As Boolean = ExecuteCode(code, operand)
            If ok Then
                WordsConsumed = If(AgcOpcodeTable.InterpretiveTakesOperand(code), 2, 1)
            End If
            Return ok
        End Function

        Private Function ExecuteCode(code As Integer, operand As Integer) As Boolean
            Select Case code
                Case &H0
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

                Case &H5A                        ' BPL: branch when MPAC >= 0
                    If ReadMpac() >= 0 Then Return GoToAddress(operand)
                    Return True

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

    End Class

End Namespace
