' ============================================================================
'  AgcMemory.vb -- the virtual AGC memory: fixed (rope) memory with bank
'  switching, and erasable memory with E-bank switching.
'
'  Fixed memory
'  ------------
'  36 banks of 1024 words.  The CPU always sees one bank at 02000..03777; which
'  bank that is depends on the operation:
'
'    010000..017777 (addresses 04000..07777)  -> banks 02 and 03 ("fixed-fixed")
'    020000..027777 (addresses 02000..03777)  -> FBANK-selected bank
'    030000..037777 (addresses 02000..03777)  -> BBANK-selected bank
'
'  Bank numbers 00 and 01 are the two halves of the 1024-word "super bank"
'  module 1; banks 010 and 011 alias 00 and 01 so that a super-bank bank
'  register value can be formed by simple arithmetic.  For the purposes of this
'  simulator, banks 00/01 hold the super-bank contents and 040..043 are mapped
'  onto them via the super-bank selection logic below.
'
'  Erasable memory
'  ---------------
'  8 banks of 256 words.  Banks 0..2 are unswitched (addresses 0000..01377);
'  banks 3..7 appear at 01400..01777 with the EBANK register selecting one.
' ============================================================================

Imports System
Imports System.Collections.Generic

Namespace Agc

    Public Class AgcMemory

        ' ------------------------------------------------------------------
        ' Banks
        ' ------------------------------------------------------------------

        ''' <summary>Physical fixed banks.  Index 0..037 for the standard bank numbers.</summary>
        Public ReadOnly Fixed(43, 1023) As Integer

        ''' <summary>10-bit fixed bank registers.  Index 0..7.</summary>
        Public ReadOnly Fbank(7) As Integer

        ''' <summary>Erasable memory.  Bank 0..7, each 256 words.</summary>
        Public ReadOnly Erasable(7, 255) As Integer

        ''' <summary>Current E-bank (0..7).</summary>
        Public Property EBank As Integer

        ''' <summary>Super-bank selection: 3 selects banks 030..037, 4 selects 040..043.</summary>
        Public Property SuperBank As Integer = 3

        Private ReadOnly _image As AgcRopeImage

        Public Sub New(image As AgcRopeImage)
            _image = image
        End Sub

        ' ------------------------------------------------------------------
        ' Initialisation
        ' ------------------------------------------------------------------

        ''' <summary>Copy the assembled rope into fixed memory and clear erasable.</summary>
        Public Sub Load()
            For b As Integer = 0 To 43
                For w As Integer = 0 To 1023
                    Dim v As Integer = 0
                    If b < AgcRopeImage.FixedBankCount Then v = _image.Fixed(b, w)
                    ' Instruction words are 16 bits wide including their top
                    ' opcode bit, so all 17 working bits are kept.
                    Fixed(b, w) = v And AgcWord.Mask17
                Next
            Next

            Array.Clear(Erasable, 0, Erasable.Length)
            Array.Clear(Fbank, 0, Fbank.Length)
            EBank = 0
        End Sub

        ' ------------------------------------------------------------------
        ' Fixed memory addressing (yaAGC semantics)
        ' ------------------------------------------------------------------

        ''' <summary>
        ''' Read a fixed memory word given the raw 15-bit "virtual address" (the
        ''' address field plus the 010000/020000/030000 prefix produced by the
        ''' instruction class).
        ''' </summary>
        Public Function ReadFixed(vaddr As Integer) As Integer
            Dim bank As Integer
            Dim offset As Integer
            DecodeFixed(vaddr, bank, offset)
            Return Fixed(bank, offset)
        End Function

        Public Sub WriteFixed(vaddr As Integer, value As Integer)
            Dim bank As Integer
            Dim offset As Integer
            DecodeFixed(vaddr, bank, offset)
            Fixed(bank, offset) = value And AgcWord.Mask17
        End Sub

        ''' <summary>
        ''' The rope-image bank number that a (module, half) pair maps to.  This
        ''' is the AGC's own numbering, so that the assembler, the rope image and
        ''' the emulator all agree:
        '''   module 0  -> banks 00 and 01   (the unswitched super-bank module)
        '''   module 2  -> banks 02 and 03   (the "fixed-fixed" area)
        '''   module 1  -> banks 030..033    (the switched super-bank module)
        '''   modules 3..17 -> banks 04..037
        ''' </summary>
        Public Shared Function LogicalBankOf(moduleNumber As Integer, half As Integer) As Integer
            If moduleNumber = 0 Then Return half
            If moduleNumber = 2 Then Return 2 + half
            If moduleNumber = 1 Then Return 24 + half          ' 030, 031
            If moduleNumber = 3 Then Return 32 + half          ' 032, 033
            Return (moduleNumber - 3) * 2 + 4 + half
        End Function

        ''' <summary>
        ''' Decode a 12-bit fixed-memory address into a rope-image bank and offset,
        ''' following the Block II bank-selection rules.  The bank number produced
        ''' is the same one the assembler uses for "BANK n".
        ''' </summary>
        Private Sub DecodeFixed(vaddr As Integer, ByRef bank As Integer, ByRef offset As Integer)
            Dim sel As Integer = vaddr And &H3FFF
            If sel >= &H2000 Then
                ' 020000..037777: the bank selected by FBANK or BBANK.  The
                ' register holds "module/2" in bits 3-5 and the half in bit 2,
                ' but a rope-image bank number is (module-1)*2 + half for the
                ' switched modules, so the two agree except for the super-bank
                ' module, whose banks are numbered 030 upwards.
                Dim selected As Integer
                If (sel And &H1000) <> 0 Then
                    ' 030000..037777 uses BBANK.
                    selected = ((Fbank(3) >> 5) And 7) * 2 + ((Fbank(4) >> 5) And 1)
                Else
                    ' 020000..027777 uses FBANK.
                    selected = ((Fbank(0) >> 5) And 7) * 2 + ((Fbank(1) >> 5) And 1)
                End If
                ' The hardware's super-bank flip-flop replaces values in the
                ' 030..033 range with the module selected by channel 7 bit 9.
                If selected >= 28 AndAlso selected <= 31 Then
                    selected = If((Fbank(2) And 2) <> 0, 24, 32) + (selected Mod 2)
                End If
                bank = selected
                offset = sel And &H3FF
            Else
                ' 04000..07777: module 2, banks 02 and 03.
                bank = 2 + ((sel >> 10) And 1)
                offset = sel And &H3FF
            End If

            If bank < 0 Then bank = 0
            If bank > AgcRopeImage.FixedBankCount - 1 Then bank = bank Mod AgcRopeImage.FixedBankCount
            If offset < 0 Then offset = 0
            If offset > 1023 Then offset = 1023
        End Sub

        ''' <summary>
        ''' Resolve a fixed address the way the assembler sees it (used by the
        ''' disassembler and by the debugger).
        ''' </summary>
        Public Function FixedWord(bank As Integer, offset As Integer) As Integer
            If bank < 0 OrElse bank > 43 Then Return 0
            If offset < 0 OrElse offset > 1023 Then Return 0
            Return Fixed(bank, offset)
        End Function

        ' ------------------------------------------------------------------
        ' Erasable memory addressing
        ' ------------------------------------------------------------------

        ''' <summary>
        ''' Read erasable memory.  Addresses 0000..01377 are unswitched; 01400..01777
        ''' are switched by the current E-bank.
        ''' </summary>
        Public Function ReadErasable(address As Integer) As Integer
            Dim a As Integer = address And &H7FF
            If a < &H300 Then
                Return Erasable(a \ &H100, a And &HFF)
            End If
            If a < &H400 Then
                ' 0300..0377 is the bank-3 window in the unswitched area; the AGC
                ' treats it as part of the switched region only when banked
                ' addressing is in use, so it is read from the current E-bank.
                Return Erasable(EBank And 7, a And &HFF)
            End If
            Return Erasable(EBank And 7, a And &HFF)
        End Function

        Public Sub WriteErasable(address As Integer, value As Integer)
            Dim a As Integer = address And &H7FF
            If a < &H300 Then
                Erasable(a \ &H100, a And &HFF) = value And AgcWord.Mask15
            Else
                Erasable(EBank And 7, a And &HFF) = value And AgcWord.Mask15
            End If
        End Sub

        ''' <summary>Read from a specific erasable bank (used by the debugger).</summary>
        Public Function ReadErasableBank(bank As Integer, offset As Integer) As Integer
            If bank < 0 OrElse bank > 7 OrElse offset < 0 OrElse offset > 255 Then Return 0
            Return Erasable(bank, offset)
        End Function

        Public Sub WriteErasableBank(bank As Integer, offset As Integer, value As Integer)
            If bank < 0 OrElse bank > 7 OrElse offset < 0 OrElse offset > 255 Then Return
            Erasable(bank, offset) = value And AgcWord.Mask15
        End Sub

        ''' <summary>Fill all of erasable memory with a value (fresh start does this).</summary>
        Public Sub FillErasable(value As Integer)
            For b As Integer = 0 To 7
                For o As Integer = 0 To 255
                    Erasable(b, o) = value And AgcWord.Mask15
                Next
            Next
        End Sub

    End Class

End Namespace
