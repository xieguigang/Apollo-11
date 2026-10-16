' ============================================================================
'  AgcOctParser.vb -- the octal literal syntax used by the OCT/2OCT pseudo-ops.
'
'  Accepts an optional sign followed by octal digits and an optional "D"
'  suffix.  A leading "-" complements the value within 15 bits, matching the
'  original assembler.
' ============================================================================

Imports System

Namespace Agc

    Public Module AgcOctParser

        Public Function TryParse(text As String, ByRef value As Integer) As Boolean
            value = 0
            If String.IsNullOrEmpty(text) Then Return False

            Dim s As String = text.Trim()
            If s.Length = 0 Then Return False

            Dim negative As Boolean = False
            Dim i As Integer = 0
            If s(0) = "+"c Then
                i = 1
            ElseIf s(0) = "-"c Then
                negative = True
                i = 1
            End If

            Dim start As Integer = i
            While i < s.Length AndAlso s(i) >= "0"c AndAlso s(i) <= "7"c
                i += 1
            End While
            If i = start Then Return False

            Dim tail As String = s.Substring(i).Trim().ToUpperInvariant()
            If tail <> "" AndAlso tail <> "D" Then Return False

            Dim raw As Integer = 0
            For k As Integer = start To i - 1
                raw = raw * 8 + (AscW(s(k)) - AscW("0"c))
            Next
            raw = raw And AgcWord.Mask15
            value = If(negative, raw Xor AgcWord.Mask15, raw)
            Return True
        End Function

    End Module

End Namespace
