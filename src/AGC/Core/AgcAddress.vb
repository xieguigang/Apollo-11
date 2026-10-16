' ============================================================================
'  AgcAddress.vb -- the assembler's segmented address model for the AGC.
'
'  The AGC has two quite different memory spaces:
'
'    * Erasable memory: 2048 15-bit words.  Banks 0, 1 and the low half of 2
'      are "unswitched" (addresses 0000..01377); banks 3..7 are "switched" and
'      appear at 01400..01777 with the EBANK register selecting the bank.
'
'    * Fixed (rope) memory: 36 banks of 1024 words.  Each bank appears in the
'      CPU address window 02000..03777; which region 02000..03777 refers to is
'      decided by bits 9-10 of the instruction word and, for the banked case,
'      by the FBANK/BBANK registers.
'
'  Inside the assembler an address is held as a bank number plus a
'  "sub-address register" (SReg) exactly as yaYUL does:
'
'      fixed, banked     SReg 02000..03777, bank in FB
'      fixed, unbanked   SReg 04000..07777 (banks 02 and 03)
'      erasable, unbanked SReg 0000..01377
'      erasable, banked   SReg 01400..01777, bank in EB
' ============================================================================

Imports System

Namespace Agc

    Public Enum AgcMemoryKind
        None = 0
        Erasable = 1
        Fixed = 2
    End Enum

    ''' <summary>A resolved (or partially resolved) assembler address.</summary>
    Public Structure AgcAddress

        ''' <summary>The sub-address register (SReg).</summary>
        Public SReg As Integer

        ''' <summary>Fixed bank number (octal 00..043) when <see cref="Banked"/>.</summary>
        Public FB As Integer

        ''' <summary>Erasable bank number (0..7) when <see cref="Banked"/>.</summary>
        Public EB As Integer

        Public Kind As AgcMemoryKind

        ''' <summary>True when the address is in a banked (switched) region.</summary>
        Public Banked As Boolean

        ''' <summary>True when the address is in an unswitched region.</summary>
        Public Unbanked As Boolean

        ''' <summary>True for a super-bank address (module 1 banks 030..043).</summary>
        Public Super As Boolean

        ''' <summary>Set when the value is a plain constant rather than memory.</summary>
        Public IsConstant As Boolean

        Public Invalid As Boolean
        Public Overflow As Boolean

        Public ReadOnly Property IsErasable As Boolean
            Get
                Return Kind = AgcMemoryKind.Erasable
            End Get
        End Property

        Public ReadOnly Property IsFixed As Boolean
            Get
                Return Kind = AgcMemoryKind.Fixed
            End Get
        End Property

        Public ReadOnly Property IsAddress As Boolean
            Get
                Return Kind = AgcMemoryKind.Erasable OrElse Kind = AgcMemoryKind.Fixed
            End Get
        End Property

        ''' <summary>Create a constant (non-address) value.</summary>
        Public Shared Function Constant(value As Integer) As AgcAddress
            Dim a As New AgcAddress()
            a.IsConstant = True
            a.SReg = value And AgcWord.Mask15
            Return a
        End Function

        ''' <summary>Create an unswitched erasable address.</summary>
        Public Shared Function UnswitchedErasable(address As Integer) As AgcAddress
            Dim a As New AgcAddress()
            a.Kind = AgcMemoryKind.Erasable
            a.Unbanked = True
            a.SReg = address
            Return a
        End Function

        ''' <summary>
        ''' The flat "pseudo address" used when a symbol value is stored in a data
        ''' word or written out in a listing.
        ''' </summary>
        Public ReadOnly Property PseudoValue As Integer
            Get
                If Unbanked Then Return SReg
                If IsErasable Then
                    ' Banked erasable keeps the hardware's 10-bit form, with the
                    ' E-bank in bits 8-9, so the flat word is just SReg.
                    Return SReg
                End If
                If IsFixed Then
                    ' Fixed banks 02..037 live at 010000 + bank*02000 in pseudo
                    ' address terms.  The super-bank module (the AGC banks 030..043)
                    ' is placed 02000 lower, at 020000..027777, so that it does not
                    ' collide with the low banks.
                    '
                    ' Note the test is on Super, not on "FB >= 24": for a banked
                    ' address FB holds the AGC bank number (030..043), not the
                    ' ordinal 24..35, so comparing it against 24 is never true.
                    Dim v As Integer = 4096 + (SReg - 2048) + (1024 * FB)
                    If Super Then v -= 2048
                    Return v
                End If
                Return SReg
            End Get
        End Property

        ''' <summary>
        ''' The 15-bit address word stored by CADR/FCADR: the fixed pseudo address
        ''' with the 010000 base removed.
        ''' </summary>
        Public ReadOnly Property CadrValue As Integer
            Get
                ' For a super-bank address the PseudoValue is encoded 02000 below
                ' the flat form (the super module sits at 020000..027777), so the
                ' offset has to be added back before the 12-bit bank offset can be
                ' taken.  Without this the CADR value is 02000 short for every
                ' symbol in banks 030..043.
                Dim p As Integer = PseudoValue
                If Super Then p += 2048
                Return p And &HFFF
            End Get
        End Property

        Public Overrides Function ToString() As String
            If Invalid Then Return "<invalid>"
            If IsConstant Then Return "const " & SReg.ToString()
            If IsErasable Then
                If Banked Then Return "E" & Convert.ToString(EB, 8) & ":" & Convert.ToString(SReg, 8)
                Return "E:" & Convert.ToString(SReg, 8)
            End If
            If IsFixed Then
                If Banked Then Return "F" & Convert.ToString(FB, 8) & ":" & Convert.ToString(SReg, 8)
                Return "F:" & Convert.ToString(SReg, 8)
            End If
            Return "?"
        End Function

    End Structure

End Namespace
