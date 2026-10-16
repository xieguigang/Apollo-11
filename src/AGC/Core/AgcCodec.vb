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
        Public Const SuperBankBase As Integer = 1536                ' 03000

        ''' <summary>
        ''' Convert a flat pseudo address into the segmented address model
        ''' (PseudoToStruct in yaYUL).
        ''' </summary>
        Public Function PseudoToAddress(value As Integer) As AgcAddress
            Dim a As New AgcAddress()
            ' The flat pseudo space runs from 000000 to 077777.  The super-bank
            ' module is placed above the low banks, so usable values extend well
            ' past 043777; rejecting them here is what turned valid symbol
            ' references into "resolved to const" errors.
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
                ' 04000..07777: unswitched fixed (banks 02 and 03).
                a.Kind = AgcMemoryKind.Fixed
                a.Unbanked = True
                a.SReg = value
            ElseIf value <= 26623 Then                       ' 063777
                ' 10000..63777: fixed, banked low banks.  FB is the AGC bank
                ' number, so bank 02 starts at pseudo 010000 and each bank is 02000
                ' wide, i.e. 1024 in the flat numbering used here.
                a.Kind = AgcMemoryKind.Fixed
                a.Banked = True
                a.SReg = 2048 + (value Mod 1024)
                a.FB = (value - 4096) \ 1024
            Else
                ' 64000..43777: fixed, the super-bank module (AGC banks 030..043).
                ' Its pseudo address is the low-bank form minus 02000 so that it
                ' does not collide with the low banks:
                '
                '   pseudo = 4096 + (SReg - 2048) + 1024*FB - 2048
                '
                ' Substituting FB = n + 24 (n = FB - 030) reduces this to
                ' pseudo = 1024*n + 26624, so n = (pseudo - 26624) / 1024.
                a.Kind = AgcMemoryKind.Fixed
                a.Banked = True
                a.Super = True
                a.FB = (value - 26624) \ 1024 + 24
                a.SReg = 2048 + ((value - 1024) Mod 1024)
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
        ''' This is simply SReg masked to the field width, with one exception.  For
        ''' an unswitched fixed address (04000..07777) SReg is the full address,
        ''' including its 04000 bit, and masking it to 12 bits keeps that bit --
        ''' which is vital, because dropping it turns "TCF 04005" into "TCF 5" and
        ''' sends the program counter out of fixed memory.  For a banked address the
        ''' bank is selected separately at run time and SReg already holds the
        ''' offset-plus-bank form the hardware wants, so it is used as it stands;
        ''' re-deriving the bank here would double-encode it.
        ''' </summary>
        Public Function BasicOperandValue(a As AgcAddress) As Integer
            Return a.SReg And &HFFF
        End Function

        ''' <summary>
        ''' A transfer instruction's target field is a full 12-bit fixed address, so
        ''' the bank-selected form must be reduced to the address the hardware will
        ''' fetch: for a banked fixed address that is the within-bank offset with
        ''' bit 02000 set to mark the 02000..03777 window.
        ''' </summary>
        Public Function TransferTargetValue(a As AgcAddress) As Integer
            If a.IsFixed AndAlso a.Banked Then
                Return (a.SReg Or &H2000) And &HFFF
            End If
            Return a.SReg And &HFFF
        End Function

        ''' <summary>
        ''' Advance an address by <paramref name="increment"/> words, rolling the
        ''' bank number when the sub-address runs off the end of the bank.
        ''' </summary>
        Public Function IncPc(input As AgcAddress, increment As Integer) As AgcAddress
            Dim result As AgcAddress = input
            If result.Invalid Then Return result

            Dim bankIncrement As Integer
            Dim inc As Integer
            If increment >= 0 Then
                bankIncrement = increment \ 4096
                inc = increment Mod 4096
            Else
                Dim mag As Integer = -increment
                bankIncrement = -(mag \ 4096)
                inc = -(mag Mod 4096)
            End If

            ' A plain constant that is small enough becomes unswitched erasable.
            If result.IsConstant Then
                If result.SReg < 0 Then Return result
                If result.SReg >= 768 Then
                    ' The location counter has drifted into a constant that is too
                    ' large to be erasable.  That state cannot be assembled, so it is
                    ' left as-is and reported by the caller.
                    Return result
                End If
                result.IsConstant = False
                result.Kind = AgcMemoryKind.Erasable
                result.Unbanked = True
                result.Banked = False
                result.EB = 0
                result.FB = 0
                result.Super = False
            End If

            If result.Overflow Then Return result

            Dim newSReg As Integer = result.SReg + inc
            result.SReg = newSReg
            If result.Banked Then
                If result.IsErasable Then
                    result.EB += bankIncrement
                ElseIf result.IsFixed Then
                    result.FB += bankIncrement
                End If
            End If

            ' Special case: a large negative offset from a fixed address can drop
            ' the location counter into erasable memory (yaYUL does the same).
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
                    minValue = 0 : maxValue = 1023               ' 00000..01777
                Else
                    minValue = 1280 : maxValue = 1535            ' 02400..02777
                End If
            ElseIf result.IsFixed Then
                If result.Banked Then
                    minValue = 2048 : maxValue = 4095            ' 04000..07777
                Else
                    ' The unswitched fixed window holds banks 02 and 03, which
                    ' together span 04000..07777.
                    minValue = 2048 : maxValue = 4095
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
