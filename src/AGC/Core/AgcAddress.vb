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

        ''' <summary>
        ''' Which 1K section of the fixed-fixed pair (AGC bank 02) an address is in.
        ''' The sources split that one 2K bank into four 1K sections -- banks 00 and
        ''' 01 below 04000 and banks 02 and 03 above it -- and sections 0 and 2 share
        ''' the sub-address 0000..0377 while 1 and 3 share 0400..0777, so the section
        ''' cannot be recovered from SReg alone.  Meaningful only for a fixed-fixed
        ''' address (FB = 2), where it is the ".binsource" bank number (0..3).
        ''' </summary>
        Public BinBank As Integer

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
        ''' Create a fixed address from a 15-bit sub-address register value.  For a
        ''' fixed address SReg is "02000 + offset", where bit 01000 selects the half
        ''' of the 2K AGC bank.
        ''' </summary>
        Public Shared Function FixedAddress(sreg As Integer) As AgcAddress
            Dim a As New AgcAddress()
            a.Kind = AgcMemoryKind.Fixed
            a.Unbanked = True
            a.SReg = sreg And AgcWord.Mask15
            Return a
        End Function

        ''' <summary>
        ''' The flat "pseudo address" used when a symbol value is stored in a data
        ''' word or written out in a listing (yaYUL's "Value" field, and its
        ''' inverse is AgcCodec.PseudoToAddress).
        ''' </summary>
        Public ReadOnly Property PseudoValue As Integer
            Get
                ' Fixed memory first: a fixed address may also carry the Unbanked
                ' flag (the fixed-fixed pair, whose flat form is below 04000), and
                ' the flat form of a fixed address is not simply SReg.
                If IsFixed Then
                    ' One AGC fixed bank is 2K words, so each bank is 02000 of the
                    ' flat space and SReg is "02000 + offset" with FB naming the
                    ' bank.  The fixed-fixed pair is bank 02 and is addressed as
                    ' "02000 + offset" for the sources' banks 02 and 03, and as
                    ' "offset" (00000..01777) for their banks 00 and 01 -- BinBank
                    ' distinguishes the two forms.  Everything else is
                    ' "010000 + 02000*(FB-04)", and the super-bank module is placed
                    ' 02000 lower because the hardware adds 010 to its F-bank field.
                    If FB = 2 Then
                        If BinBank >= 2 Then
                            Return 2048 + (SReg And 2047)
                        End If
                        Return 1024 * (BinBank And 1) + (SReg And 1023)
                    End If
                    Dim v As Integer = 4096 + (SReg - 2048) + 2048 * (FB - 4)
                    If Super Then
                        ' FB is the hardware bank number, but the sources' super-bank
                        ' numbering is offset by eight, so the flat block is derived
                        ' from the source number.
                        Dim src As Integer = FB + 8
                        v = 12288 + 2048 * (src - 32) + (SReg - 2048)
                    End If
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
                ' One AGC bank is 2K, so the 12-bit word address of an offset inside
                ' a bank is just "010000 + offset" with the base removed.  The
                ' super-bank module sits at 030000, so its addresses are already in
                ' that form once the 02000 the flat form subtracted is added back.
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
