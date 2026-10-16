' ============================================================================
'  RopeImage.vb -- the assembled AGC memory image.
'
'  Fixed (rope) memory is 36 banks of 1024 words; the CPU sees one bank at a
'  time in the window 02000..03777.  Erasable memory is 8 banks of 256 words;
'  banks 0..2 are unswitched and banks 3..7 are switched by EBANK.
' ============================================================================

Imports System
Imports System.Collections.Generic

Namespace Agc

    Public Class AgcRopeImage

        Public Const FixedBankCount As Integer = 44      ' banks 00..043 (octal)
        ''' <summary>
        ''' Words in one fixed bank.  An AGC fixed bank is a 2K bank: words 0..01777
        ''' appear at CPU addresses 02000..03777 and, for banks 2 and 3 only, at
        ''' 04000..07777.  Accessing a bank through a 1K half is done with
        ''' <see cref="SetFixedHalf"/> / <see cref="GetFixedHalf"/>, which is the
        ''' form the published ".binsource" listings use.
        ''' </summary>
        Public Const FixedBankWords As Integer = 2048
        Public Const FixedHalfWords As Integer = 1024
        Public Const ErasableBankCount As Integer = 8
        Public Const ErasableBankWords As Integer = 256

        ''' <summary>Fixed memory: bank * 2048 + offset (offset is 0..01777 within the bank).</summary>
        Public ReadOnly Fixed(,) As Integer = New Integer(FixedBankCount - 1, FixedBankWords - 1) {}

        ''' <summary>Which words of fixed memory were actually written by the assembler.</summary>
        Public ReadOnly FixedUsed(,) As Boolean = New Boolean(FixedBankCount - 1, FixedBankWords - 1) {}

        ''' <summary>Erasable memory: bank * 256 + offset.</summary>
        Public ReadOnly Erasable(,) As Integer = New Integer(ErasableBankCount - 1, ErasableBankWords - 1) {}

        ''' <summary>Bank checksums produced by the assembler (one's-complement sum of words).</summary>
        Public ReadOnly FixedChecksum(FixedBankCount - 1) As Integer

        ''' <summary>Per-bank word counts, for reporting.</summary>
        Public ReadOnly FixedWordCount(FixedBankCount - 1) As Integer

        ''' <summary>Highest offset written in each bank (for "bank full" reporting).</summary>
        Public ReadOnly FixedHighWater(FixedBankCount - 1) As Integer

        Public Sub New()
            Clear()
        End Sub

        Public Sub Clear()
            Array.Clear(Fixed, 0, Fixed.Length)
            Array.Clear(FixedUsed, 0, FixedUsed.Length)
            Array.Clear(Erasable, 0, Erasable.Length)
            Array.Clear(FixedChecksum, 0, FixedChecksum.Length)
            Array.Clear(FixedWordCount, 0, FixedWordCount.Length)
            WrittenCount = 0
            For i As Integer = 0 To FixedBankCount - 1
                FixedHighWater(i) = -1
            Next
        End Sub

        ''' <summary>Number of words written into fixed memory (diagnostic).</summary>
        Public Property WrittenCount As Integer

        ''' <summary>
        ''' Store a word in fixed memory.  <paramref name="offset"/> is the
        ''' sub-address inside the 2K bank (0000..01777).
        ''' </summary>
        Public Sub SetFixed(bank As Integer, offset As Integer, word As Integer)
            Dim o As Integer = offset And &H7FF
            If bank < 0 OrElse bank >= FixedBankCount OrElse o < 0 OrElse o >= FixedBankWords Then Return
            Fixed(bank, o) = word And AgcWord.Mask17
            FixedUsed(bank, o) = True
            FixedWordCount(bank) += 1
            WrittenCount += 1
            If o > FixedHighWater(bank) Then FixedHighWater(bank) = o
        End Sub

        ''' <summary>
        ''' Store a word in one 1K half of a fixed bank, using the layout of the
        ''' published ".binsource" listings: half 0 is the first 1024 words the CPU
        ''' sees in that bank, half 1 the second.
        ''' </summary>
        Public Sub SetFixedHalf(bank As Integer, half As Integer, offset As Integer, word As Integer)
            SetFixed(bank, (half And 1) * FixedHalfWords + (offset And &H3FF), word)
        End Sub

        Public Function GetFixed(bank As Integer, offset As Integer) As Integer
            Dim o As Integer = offset And &H7FF
            If bank < 0 OrElse bank >= FixedBankCount OrElse o < 0 OrElse o >= FixedBankWords Then Return 0
            Return Fixed(bank, o)
        End Function

        ''' <summary>Read one 1K half of a fixed bank (".binsource" layout).</summary>
        Public Function GetFixedHalf(bank As Integer, half As Integer, offset As Integer) As Integer
            Return GetFixed(bank, (half And 1) * FixedHalfWords + (offset And &H3FF))
        End Function

        ''' <summary>True if any word was written into fixed memory bank <paramref name="bank"/>.</summary>
        Public Function BankHasContent(bank As Integer) As Boolean
            For o As Integer = 0 To FixedBankWords - 1
                If FixedUsed(bank, o) Then Return True
            Next
            Return False
        End Function

        ''' <summary>Recompute per-bank one's-complement checksums over the whole bank.</summary>
        Public Sub ComputeChecksums()
            For b As Integer = 0 To FixedBankCount - 1
                Dim sum As Integer = 0
                For o As Integer = 0 To FixedBankWords - 1
                    sum = AgcWord.Add(sum, Fixed(b, o))
                Next
                FixedChecksum(b) = sum
            Next
        End Sub

        ''' <summary>
        ''' Serialise the rope image in the ".binsource" octal format used by
        ''' virtualagc, so that an assembled image can be diffed against the
        ''' published rope listings.  A listing holds 1024 words per "BANK="
        ''' section, so each AGC bank is written as two consecutive banks (which is
        ''' the numbering the published Comanche055/AP11ROPE listings use).
        ''' </summary>
        Public Function ToBinSource(includeErasable As Boolean) As String
            Dim sb As New Text.StringBuilder()
            sb.AppendLine("; AGC rope image generated by the VB.NET AGC assembler")
            For b As Integer = 0 To FixedBankCount - 1
                For half As Integer = 0 To 1
                    If Not HalfHasContent(b, half) Then Continue For
                    sb.AppendLine()
                    sb.AppendLine("BANK=" & (2 * b + half).ToString())
                    sb.AppendLine()
                    ListHalf(sb, b, half)
                Next
            Next
            If includeErasable Then
                For b As Integer = 0 To ErasableBankCount - 1
                    sb.AppendLine()
                    sb.AppendLine("BANK=" & b.ToString())
                    sb.AppendLine()
                    Dim o As Integer = 0
                    While o < ErasableBankWords
                        Dim parts As New List(Of String)()
                        For k As Integer = 0 To 7
                            If o + k < ErasableBankWords Then parts.Add(AgcWord.ToOctal(Erasable(b, o + k)))
                        Next
                        sb.AppendLine(String.Join(" ", parts.ToArray()))
                        o += 8
                    End While
                Next
            End If
            Return sb.ToString()
        End Function

        Private Function HalfHasContent(bank As Integer, half As Integer) As Boolean
            Dim base As Integer = half * FixedHalfWords
            For o As Integer = 0 To FixedHalfWords - 1
                If FixedUsed(bank, base + o) Then Return True
            Next
            Return False
        End Function

        Private Sub ListHalf(sb As Text.StringBuilder, bank As Integer, half As Integer)
            Dim base As Integer = half * FixedHalfWords
            Dim o As Integer = 0
            While o < FixedHalfWords
                Dim parts As New List(Of String)()
                For k As Integer = 0 To 7
                    If o + k < FixedHalfWords Then
                        parts.Add(AgcWord.ToOctal(Fixed(bank, base + o + k)))
                    End If
                Next
                sb.AppendLine(String.Join(" ", parts.ToArray()))
                o += 8
            End While
        End Sub

    End Class

End Namespace
