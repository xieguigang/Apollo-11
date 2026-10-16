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

        ''' <summary>Number of interpretive instructions executed.</summary>
        Public Property InstructionCount As Integer

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

        Private Function ReadErasable(a As Integer) As Integer
            Return _cpu.GetErasable(a And &HFFF) And AgcWord.Mask15
        End Function

        Private Sub WriteErasable(a As Integer, value As Integer)
            _cpu.SetErasable(a And &HFFF, value And AgcWord.Mask15)
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

        ''' <summary>
        ''' Read MPAC as a signed integer.  Each MPAC word holds a 14-bit
        ''' one's-complement magnitude whose sign lives in bit 14 (040000).
        ''' </summary>
        Private Function ReadMpac() As Integer
            ' A negative MPAC is the 28-bit one's complement of its magnitude, so
            ' the sign bits sit in the top bits of the high word and must be tested
            ' before the value is narrowed.
            Dim rawHi As Integer = ReadErasable(AddrMpac)
            Dim rawLo As Integer = ReadErasable(AddrMpacLow)
            If (rawHi And &H2000) <> 0 Then
                Dim cHi As Integer = (MpacModulus - 1) - rawHi
                Dim cLo As Integer = (MpacModulus - 1) - rawLo
                Return -(((cHi And &H3FFF) << 14) Or (cLo And &H3FFF))
            End If
            Return ((rawHi And &H3FFF) << 14) Or (rawLo And &H3FFF)
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
        ''' Store a signed integer into MPAC.  Negating adds 040000 to each word,
        ''' which places the sign bits in the top bits of the high word.
        ''' </summary>
        Private Sub WriteMpac(value As Integer)
            Dim mag As Long = Math.Abs(CLng(value))
            Dim hi As Integer = CInt((mag >> 14) And &H3FFF)
            Dim lo As Integer = CInt(mag And &H3FFF)
            If value < 0 Then
                hi = (hi + MpacModulus) And &H3FFF
                lo = (lo + MpacModulus) And &H3FFF
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
        Public Function Execute(word As Integer, operand As Integer) As Boolean
            InstructionCount += 1

            ' The instruction word is
            '     077777 & ~((code + 1) | ((code2 + 1) << 7))
            ' The complement clears the bits of (code+1) in 0-6 and of (code2+1)
            ' in 7-13.  Because the two halves are combined with OR and the
            ' operands are never all-ones in a way that overlaps, XOR with the
            ' original mask recovers each half exactly.
            Dim halves As Integer = AgcWord.Mask15 Xor (word And AgcWord.Mask15)
            Dim code As Integer = (halves And &H7F) - 1
            Dim code2 As Integer = ((halves >> 7) And &H7F) - 1

            If TraceEnabled Then
                Console.WriteLine("    INTERP word=" & AgcWord.ToOctal(word) &
                                  " operand=" & AgcWord.ToOctal(operand) &
                                  " code=" & code.ToString() & " code2=" & code2.ToString())
            End If

            If code = 0 AndAlso code2 >= 0 Then code = code2
            If code < 0 Then Return False

            Return ExecuteCode(code, operand)
        End Function

        Private Function ExecuteCode(code As Integer, operand As Integer) As Boolean
            Select Case code
                Case &H0
                    ' EXIT, and the STORE family's base code.  A pure EXIT means the
                    ' interpretive sequence is finished: return to basic instructions
                    ' at the word after the operand.
                    Loc = (_cpu.Z + 1) And &HFFF
                    Return True

                Case &H19, &H1B                   ' DLOAD / DLOAD*
                    Dim hi As Integer = ReadOperand(operand - 1)
                    Dim lo As Integer = ReadOperand(operand)
                    WriteErasable(AddrMpac, hi)
                    WriteErasable(AddrMpacLow, lo)
                    Return True

                Case &H71, &H73                   ' DAD / DAD*
                    Return DoubleAdd(operand, 1)

                Case &H69, &H6B                   ' DSU / DSU*
                    Return DoubleAdd(operand, -1)

                Case &H79, &H7B                   ' DMP / DMP*
                    Return DoubleMultiply(operand)

                Case &H1, &H3                     ' VLOAD / VLOAD*
                    WriteErasable(AddrMpac, ReadOperand(operand))
                    WriteErasable(AddrMpacLow, 0)
                    Return True

                Case &H49                        ' VADD
                    Return VectorAdd(operand, 1)

                Case &H4B                        ' VSUB
                    Return VectorAdd(operand, -1)

                Case &H40                        ' DCOMP: negate MPAC
                    WriteMpac(-ReadMpac())
                    Return True

                Case &H78                        ' PUSH
                    PushDown(ReadErasable(AddrMpac))
                    Return True

                Case &H56                        ' GOTO
                    _cpu.Z = operand And &HFFF
                    Return True

                Case &H0                          ' unreachable; kept for clarity
                    Return False

                Case Else
                    Return False
            End Select
        End Function

        ' ------------------------------------------------------------------
        ' Double-precision arithmetic
        ' ------------------------------------------------------------------

        ''' <summary>
        ''' MPAC = MPAC +/- the 28-bit value at (operand-1, operand).  DLOAD, DAD and
        ''' DSU address the low word of a double-precision argument, so the high word
        ''' is one address below it.
        ''' </summary>
        Private Function DoubleAdd(operand As Integer, sign As Integer) As Boolean
            Dim hi As Integer = ReadOperand(operand - 1)
            Dim lo As Integer = ReadOperand(operand)
            Dim other As Integer = (hi << 14) Or lo
            Dim mine As Integer = ReadMpac()
            If sign < 0 Then other = -other
            WriteMpac(mine + other)
            Return True
        End Function

        ''' <summary>MPAC = MPAC * the double-precision value at the operand.</summary>
        Private Function DoubleMultiply(operand As Integer) As Boolean
            Dim hi As Integer = ReadOperand(operand - 1)
            Dim lo As Integer = ReadOperand(operand)
            Dim other As Integer = (hi << 14) Or lo
            Dim product As Long = CLng(ReadMpac()) * CLng(other)
            ' The result keeps the top 28 bits.
            WriteMpac(CInt(product >> 28))
            Return True
        End Function

        ''' <summary>MPAC = MPAC +/- the 15-bit value at the operand.</summary>
        Private Function VectorAdd(operand As Integer, sign As Integer) As Boolean
            Dim v As Integer = ReadSigned(operand)
            If sign < 0 Then v = -v
            WriteMpac(ReadMpac() + (v << 14))
            Return True
        End Function

    End Class

End Namespace
