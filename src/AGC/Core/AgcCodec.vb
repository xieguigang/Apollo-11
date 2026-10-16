' ============================================================================
'  AgcCodec.vb -- conversion between "pseudo addresses" (the flat numbering
'  used in AGC assembly source) and the assembler's segmented address model,
'  plus the program-counter increment rules.
'
'  Pseudo address ranges (Block II):
'      00000..01377   unswitched erasable (banks 0, 1 and the low half of 2)
'      01400..03777   switched erasable   (EBANK selects bank 3..7)
'      04000..07777   fixed, unswitched   (banks 02 and 03)
'      10000..21777   fixed, banked       (bank = (value-10000)/2000)
'      30000..43777   fixed, super banked (bank = 030 + (value-30000)/2000)
'
'  Note that the assembler writes fixed banked addresses as
'  "010000 + (bank * 02000) + offset", which means bank 030 in a super bank is
'  written as 100000 (octal) rather than 110000.
' ============================================================================

Imports System

Namespace Agc

    Public Module AgcCodec

        Public Const ErasableUnswitchedLimit As Integer = 767       ' 01377
        Public Const ErasableSwitchedLimit As Integer = 2047        ' 03777
        Public Const FixedUnswitchedLimit As Integer = 4095         ' 07777
        Public Const LowBankedFixedLimit As Integer = 12287         ' 027777
        Public Const SuperBankBase As Integer = 12288               ' 030000

        ''' <summary>
        ''' Convert a flat pseudo address into the segmented address model
        ''' (PseudoToStruct in yaYUL).
        '''
        ''' The invariant that everything else depends on: for a fixed address SReg
        ''' is always "02000 + offset", where offset runs 0..01777 inside a 2K AGC
        ''' bank, and the AGC bank number is in FB.  Because an instruction's
        ''' operand field is exactly "SReg &amp; 07777", the assembled word is the
        ''' address the hardware needs: bit 01000 of SReg chooses which half of the
        ''' bank the operand addresses, and the bank itself is selected at run time
        ''' by FBANK/BBANK or, for 04000..07777, by that same bit.
        ''' </summary>
        Public Function PseudoToAddress(value As Integer) As AgcAddress
            Dim a As New AgcAddress()
            ' The flat pseudo space runs from 000000 to 077777.
            If value < 0 OrElse value > AgcWord.Mask15 Then
                a.Invalid = True
                Return a
            End If

            If value <= ErasableUnswitchedLimit Then
                ' 00000..01377: unswitched erasable.
                a.Kind = AgcMemoryKind.Erasable
                a.Unbanked = True
                a.SReg = value
            ElseIf value <= ErasableSwitchedLimit Then
                ' 01400..03777: switched erasable.  The hardware encodes the E-bank
                ' in bits 8-9 of the 10-bit address, so E-bank 2 with low byte 0377
                ' is 02777.  The assembler keeps the same form in SReg, which is why
                ' AgcAddress.PseudoValue for banked erasable is (EB << 8) | low byte.
                a.Kind = AgcMemoryKind.Erasable
                a.Banked = True
                a.SReg = value                                   ' E-bank in bits 8-9
                a.EB = (value >> 8) And 7
            ElseIf value <= FixedUnswitchedLimit Then
                ' 04000..07777: the fixed-fixed pair.  It is the whole of AGC bank
                ' 02, which the sources also address in its lower half as banks 00 and
                ' 01.  SReg is "02000 + offset" and BinBank records which of the four
                ' 1K sections the published listings use, so the flat form survives a
                ' round trip.
                a.Kind = AgcMemoryKind.Fixed
                a.Banked = True
                a.FB = 2
                a.BinBank = 2 + ((value >> 10) And 1)
                a.SReg = 2048 + (value And 2047)
            ElseIf value <= SuperBankBase Then
                ' 010000..027777: fixed, banked, the ordinary switchable banks.
                ' Each 2K bank gets one 02000 block, and the hardware bank number is
                ' 04 + (value - 010000)/02000.
                a.Kind = AgcMemoryKind.Fixed
                a.Banked = True
                a.FB = 4 + (value - 4096) \ 2048
                a.SReg = 2048 + (value And 2047)
            Else
                ' 030000..043777: the super-bank module.  The sources number these
                ' banks 030..043, while the hardware names them 024..035 by adding
                ' the super-bank bits of channel 7 to the F-bank field.  Mapping the
                ' source number onto the hardware one is therefore a subtraction of
                ' eight, which is what "SourceBankOf"/"HardwareBankOf" do.
                a.Kind = AgcMemoryKind.Fixed
                a.Banked = True
                a.Super = True
                ' The sources' super-bank numbers are 030..043; the hardware bank is
                ' the source number minus eight, i.e. "032..035 + 024..031".
                Dim src As Integer = 32 + (value - 12288) \ 2048
                a.FB = src - 8
                a.SReg = 2048 + (value And 2047)
            End If
            a.SReg = a.SReg And AgcWord.Mask15
            Return a
        End Function

        ''' <summary>Flatten a segmented address back to its pseudo address.</summary>
        Public Function AddressToPseudo(a As AgcAddress) As Integer
            Return a.PseudoValue
        End Function

        ''' <summary>
        ''' The value an instruction's 12-bit operand field takes for a basic-set
        ''' address such as TC or TCF.
        '''
        ''' This is "SReg &amp; 07777".  For a fixed address SReg is "02000 + offset"
        ''' inside a 2K bank, so the operand carries the offset and bit 01000
        ''' selects the half of the bank; the bank register supplies the rest of
        ''' the address at run time.  An unswitched erasable address (0000..01777)
        ''' is already its own operand, and a switched one carries the E-bank in
        ''' bits 8-9.
        ''' </summary>
        Public Function BasicOperandValue(a As AgcAddress) As Integer
            Return a.SReg And &HFFF
        End Function

        ''' <summary>
        ''' A transfer instruction's target field is the same 12-bit operand as any
        ''' other: the hardware masks the fetched instruction to 12 bits and forms
        ''' the address from the low 11 bits plus the bank selection.  Keeping the
        ''' 01000 bit is what makes "TCF 04005" reach 04005 rather than dropping
        ''' into 02005.
        ''' </summary>
        Public Function TransferTargetValue(a As AgcAddress) As Integer
            Return a.SReg And &HFFF
        End Function

        ''' <summary>
        ''' Advance an address by <paramref name="increment"/> words, rolling the
        ''' bank number when the sub-address runs off the end of the bank.
        ''' </summary>
        Public Function IncPc(input As AgcAddress, increment As Integer) As AgcAddress
            Dim result As AgcAddress = input
            If result.Invalid Then Return result

            ' yaYUL splits an increment into a bank part (bits 12 and above) and a
            ' sub-address part.  The Luminary sources contain decrements of about
            ' 012000, so this split matters.
            Dim bankIncrement As Integer
            If increment >= 0 Then
                bankIncrement = (increment >> 12) And 7
                increment = increment And &HFFF
            Else
                Dim mag As Integer = -increment
                bankIncrement = -((mag >> 12) And 7)
                increment = -(mag And &HFFF)
            End If

            ' A plain constant becomes unswitched erasable if it is small enough.
            If result.IsConstant Then
                If result.SReg < 0 Then Return result
                If result.SReg >= &H400 Then Return result
                result.IsConstant = False
                result.Kind = AgcMemoryKind.Erasable
                result.Unbanked = True
                result.Banked = False
                result.EB = 0
                result.FB = 0
                result.Super = False
            End If

            If result.Overflow Then Return result

            ' The fixed-fixed pair is one 2K AGC bank split into four 1K sections:
            ' SReg is the offset inside its section, and the section number lives in
            ' BinBank (the sources' banks 00..03).  Advancing stays inside the
            ' section unless it runs off the end, in which case it moves to the next.
            If result.IsFixed AndAlso result.FB = 2 Then
                Dim newOffset As Integer = (result.SReg And &H3FF) + increment
                If newOffset < 0 Then
                    result.Overflow = True
                    result.SReg = 0
                    Return result
                End If
                If newOffset > 1023 Then
                    If newOffset > 2047 Then
                        result.Overflow = True
                        newOffset = 1023
                    End If
                    result.BinBank = (result.BinBank + 1) And 3
                    newOffset -= 1024
                End If
                result.SReg = newOffset And &H3FF
                Return result
            End If

            Dim newSReg As Integer = result.SReg + increment
            result.SReg = newSReg
            If result.IsErasable AndAlso result.Banked Then
                result.EB += bankIncrement
            ElseIf result.IsFixed AndAlso result.Banked Then
                result.FB += bankIncrement
            End If

            ' A large negative offset from a fixed address can drop the location
            ' counter into unswitched erasable (yaYUL does the same).
            If result.IsFixed AndAlso bankIncrement <> 0 AndAlso result.FB = 0 Then
                result.Kind = AgcMemoryKind.Erasable
                result.Banked = False
                result.Unbanked = True
                result.EB = 0
                result.FB = 0
                result.Super = False
            End If

            Dim minValue As Integer, maxValue As Integer
            If result.IsErasable Then
                If result.Unbanked Then
                    minValue = 0 : maxValue = &H5FF              ' 00000..01377
                ElseIf result.Banked Then
                    minValue = &H600 : maxValue = &H7FF          ' 01400..01777
                Else
                    result.Invalid = True
                    Return result
                End If
            ElseIf result.IsFixed Then
                If result.Banked Then
                    minValue = &H800 : maxValue = &HFFF          ' 02000..03777
                ElseIf result.Unbanked Then
                    minValue = &H800 : maxValue = &HFFF          ' 04000..07777
                Else
                    result.Invalid = True
                    Return result
                End If
            Else
                result.Invalid = True
                Return result
            End If

            If result.SReg < minValue Then
                result.Overflow = True
                result.SReg = minValue
            ElseIf result.SReg > maxValue Then
                result.Overflow = True
                result.SReg = maxValue
            End If

            Return result
        End Function

        ''' <summary>
        ''' The 15-bit value stored for a fixed address reference (GENADR/ADRES)
        ''' is the sub-address register, i.e. the offset within the bank.
        ''' </summary>
        Public Function FixedSubAddress(a As AgcAddress) As Integer
            Return a.SReg And AgcWord.Mask15
        End Function

    End Module

End Namespace
