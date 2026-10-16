' ============================================================================
'  BinSource.vb -- read and write the ".binsource" octal rope listings used by
'  the virtualagc project.
'
'  The format is a sequence of banks:
'
'      BANK=n
'
'      00004 34054 56006 12526 52011 00006 31311 52006
'      ...
'
'  Each line holds up to eight 5-digit octal words.  The words are listed in
'  the order the CPU sees them within the bank, i.e. starting at the bank's
'  base address.  A "@" marks a word that was never written.
' ============================================================================

Imports System
Imports System.Collections.Generic
Imports System.IO

Namespace Agc

    Public Class AgcBinSource

        ''' <summary>Words per fixed bank, indexed by bank number.</summary>
        Public ReadOnly Banks As New Dictionary(Of Integer, Integer())()
        ''' <summary>Records the order in which banks appear.</summary>
        Public ReadOnly BankOrder As New List(Of Integer)()
        ''' <summary>Lines that could not be parsed.</summary>
        Public ReadOnly Errors As New List(Of String)()

        Public Sub Load(path As String)
            Banks.Clear()
            BankOrder.Clear()
            Errors.Clear()

            Dim current As Integer = -1
            Dim words As New List(Of Integer)()

            For Each raw As String In File.ReadAllLines(path)
                Dim line As String = raw.Trim()
                If line.Length = 0 Then Continue For
                If line.StartsWith(";", StringComparison.Ordinal) Then Continue For

                If line.StartsWith("BANK=", StringComparison.OrdinalIgnoreCase) Then
                    Flush(current, words)
                    Dim text As String = line.Substring(5).Trim()
                    Dim bankNo As Integer
                    If Integer.TryParse(text, bankNo) Then
                        current = bankNo
                    Else
                        Errors.Add("Bad BANK line: " & line)
                        current = -1
                    End If
                    Continue For
                End If

                If current < 0 Then Continue For

                For Each token As String In line.Split(" "c, Chr(9), Chr(13))
                    If token.Length = 0 Then Continue For
                    If token = "@" Then
                        words.Add(-1)
                        Continue For
                    End If
                    Dim v As Integer
                    If Integer.TryParse(token, v) Then
                        words.Add(v)
                    Else
                        Errors.Add("Bad word '" & token & "' in bank " & current.ToString())
                    End If
                Next
            Next

            Flush(current, words)
        End Sub

        Private Sub Flush(bank As Integer, words As List(Of Integer))
            If bank >= 0 AndAlso words.Count > 0 Then
                If Not Banks.ContainsKey(bank) Then BankOrder.Add(bank)
                Dim arr As Integer() = words.ToArray()
                If Banks.ContainsKey(bank) Then
                    Dim existing As Integer() = Banks(bank)
                    Dim size As Integer = Math.Max(existing.Length, arr.Length)
                    Dim merged As Integer() = New Integer(size - 1) {}
                    Array.Copy(existing, merged, existing.Length)
                    For i As Integer = 0 To arr.Length - 1
                        merged(i) = arr(i)
                    Next
                    Banks(bank) = merged
                Else
                    Banks(bank) = arr
                End If
            End If
            words.Clear()
        End Sub

        Public Function BankCount() As Integer
            Return Banks.Count
        End Function

    End Class

    ''' <summary>Compares an assembled rope image against a reference listing.</summary>
    Public Class AgcRopeComparer

        Public Property MismatchLimit As Integer = 40
        Public ReadOnly Mismatches As New List(Of String)()
        Public Property SameCount As Integer
        Public Property DifferCount As Integer
        Public Property ReferenceOnlyCount As Integer
        Public Property AssembledOnlyCount As Integer

        Public Function Compare(assembled As AgcRopeImage, reference As AgcBinSource) As Boolean
            Dim ok As Boolean = True

            For Each bank As Integer In reference.BankOrder
                Dim ref As Integer() = reference.Banks(bank)
                ' Fixed banks hold 1024 words; the first word of the listing is
                ' the first word of the bank.
                For offset As Integer = 0 To ref.Length - 1
                    Dim expected As Integer = ref(offset)
                    If expected < 0 Then Continue For          ' "@" = unused
                    Dim actual As Integer = 0
                    Dim sreg As Integer = 2048 + offset        ' 04000 + offset
                    If bank < AgcRopeImage.FixedBankCount AndAlso offset < AgcRopeImage.FixedBankWords Then
                        actual = assembled.Fixed(bank, offset)
                    End If
                    If actual = (expected And AgcWord.Mask15) Then
                        SameCount += 1
                    Else
                        DifferCount += 1
                        ok = False
                        If Mismatches.Count < MismatchLimit Then
                            Mismatches.Add(String.Format("bank {0:00} offset {1:0000} (SReg {2}): expected {3}, assembled {4}",
                                                         bank, offset, AgcWord.ToOctal(sreg),
                                                         AgcWord.ToOctal(expected), AgcWord.ToOctal(actual)))
                        End If
                    End If
                Next
            Next

            Return ok
        End Function

    End Class

End Namespace
