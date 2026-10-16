' ============================================================================
'  DebugProbe.vb -- temporary development diagnostics.
' ============================================================================

Imports System
Imports System.Collections.Generic
Imports System.IO

Namespace Agc

    Public Module DebugProbe

        ''' <summary>Print how the source reader classifies selected raw lines.</summary>
        Public Sub DumpLines(path As String, lineNumbers As Integer())
            If Not File.Exists(path) Then
                Console.WriteLine("PROBE file not found: " & path)
                Return
            End If
            Dim raw As String() = File.ReadAllLines(path)
            For Each n As Integer In lineNumbers
                If n < 1 OrElse n > raw.Length Then Continue For
                Dim text As String = raw(n - 1)
                ' Show the raw field split first, using the same logic as the reader.
                Dim manual As String = text
                Dim h As Integer = manual.IndexOf("#"c)
                If h >= 0 Then manual = manual.Substring(0, h)
                Dim parts As New List(Of String)()
                Dim j As Integer = 0
                While j < manual.Length AndAlso parts.Count < 6
                    While j < manual.Length AndAlso Char.IsWhiteSpace(manual(j))
                        j += 1
                    End While
                    If j >= manual.Length Then Exit While
                    Dim st As Integer = j
                    While j < manual.Length AndAlso Not Char.IsWhiteSpace(manual(j))
                        j += 1
                    End While
                    parts.Add(manual.Substring(st, j - st))
                End While
                Console.WriteLine("RAWSPLIT " & n & ": " & String.Join("|", parts.ToArray()))

                Dim l As AgcLine = AgcSourceReader.ParseLine(text, path, n)
                Dim desc As String
                If l Is Nothing Then
                    desc = "<null>"
                Else
                    desc = "op=[" & l.Opcode & "] operand=[" & l.Operand & "]"
                End If
                Console.WriteLine("PROBE " & n & ": len=" & text.Length & " [" & text.Replace(Chr(9), "<T>") & "] -> " & desc &
                                  " firstStart=" & AgcSourceReader.DiagnosticFirstStart &
                                  " fields=" & AgcSourceReader.DiagnosticFieldCount)
            Next
        End Sub

    End Module

End Namespace
