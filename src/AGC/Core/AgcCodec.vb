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
            If value < 0 OrElse value > 18431 Then          ' 043777
                a.Invalid = True
                Return a
            End If

            If value <= ErasableUnswitchedLimit Then
                ' 00000..01377: unswitched erasable.
                a.Kind = AgcMemoryKind.Erasable
                a.Unbanked = True
                a.SReg = value
            ElseIf value <= ErasableSwitchedLimit Then
                ' 01400..03777: switched erasable.  This area is five 256-word
                ' blocks mapping onto E-banks 3..7; the first two blocks of the
                ' switched window belong to banks 0..2, which are addressed as
                ' part of the unswitched area.
                Dim offset As Integer = value - 1024
                a.Kind = AgcMemoryKind.Erasable
                a.Banked = True
                a.SReg = 5120 + (offset Mod 256)              ' 01400 + low byte
                a.EB = 2 + (offset \ 256)
            ElseIf value <= FixedUnswitchedLimit Then
                ' 04000..07777: unswitched fixed (banks 02 and 03).
                a.Kind = AgcMemoryKind.Fixed
                a.Unbanked = True
                a.SReg = value
            ElseIf value <= 9215 Then                        ' 021777
                ' 10000..21777: fixed, banked.
                a.Kind = AgcMemoryKind.Fixed
                a.Banked = True
                a.SReg = 2048 + (value Mod 1024)
                a.FB = (value - 4096) \ 1024
            Else
                ' 30000..43777: fixed, super banked (banks 030..043).
                a.Kind = AgcMemoryKind.Fixed
                a.Banked = True
                a.Super = True
                a.SReg = 2048 + (value Mod 1024)
                a.FB = 24 + ((value - SuperBankBase) \ 1024)
            End If
            a.SReg = a.SReg And AgcWord.Mask15
            Return a
        End Function

        ''' <summary>Flatten a segmented address back to its pseudo address.</summary>
        Public Function AddressToPseudo(a As AgcAddress) As Integer
            Return a.PseudoValue
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
                If result.SReg >= 768 Then Return result          ' 01400
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
                    minValue = 0 : maxValue = 767                ' 01377
                Else
                    minValue = 1280 : maxValue = 1535            ' 02400..02777
                    result.EB += bankIncrement
                End If
            ElseIf result.IsFixed Then
                If result.Banked Then
                    minValue = 2048 : maxValue = 4095            ' 04000..07777
                    result.FB += bankIncrement
                Else
                    minValue = 4096 : maxValue = 8191            ' 010000..017777
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
