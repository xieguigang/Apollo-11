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
                    Fixed(b, w) = v And AgcWord.Mask15
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
            Fixed(bank, offset) = value And AgcWord.Mask15
        End Sub

        Private Sub DecodeFixed(vaddr As Integer, ByRef bank As Integer, ByRef offset As Integer)
            Dim sel As Integer = vaddr And &H3FFF
            If sel >= &H2000 Then
                ' 020000..037777: FBANK or BBANK selected bank.
                If (vaddr And &H1000) <> 0 Then
                    ' 030000..037777 uses the BBANK (Fbank[3]) register.
                    bank = ((Fbank(3) And 7) << 3) Or Fbank(1 + ((sel >> 10) And 1))
                Else
                    bank = ((Fbank(0) And 7) << 3) Or Fbank(1 + ((sel >> 10) And 1))
                End If
                offset = sel And &H3FF
            Else
                ' 04000..07777 maps directly onto banks 02 and 03.
                bank = 2 + ((sel >> 10) And 1)
                offset = sel And &H3FF
            End If

            If bank < 0 Then bank = 0
            If bank > 43 Then bank = bank And 43
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
