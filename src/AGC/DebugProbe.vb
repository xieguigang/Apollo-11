' ============================================================================
'  DebugProbe.vb -- temporary development diagnostics.
' ============================================================================

Imports System
Imports System.Collections.Generic
Imports System.IO

Namespace Agc

    Public Module DebugProbe

        ''' <summary>
        ''' Print how the source reader classifies selected raw lines, including
        ''' how a numeric operand resolves to a segmented address.
        ''' </summary>
        Public Sub DumpLines(path As String, lineNumbers As Integer())
            If Not File.Exists(path) Then
                Console.WriteLine("PROBE file not found: " & path)
                Return
            End If
            Dim raw As String() = File.ReadAllLines(path)
            For Each n As Integer In lineNumbers
                If n < 1 OrElse n > raw.Length Then Continue For
                Dim text As String = raw(n - 1)
                Dim l As AgcLine = AgcSourceReader.ParseLine(text, path, n)
                Dim desc As String
                If l Is Nothing Then
                    desc = "<null>"
                Else
                    desc = "label=[" & l.Label & "] op=[" & l.Opcode & "] operand=[" & l.Operand & "]"
                    Dim v As Integer
                    If AgcWord.TryParseInteger(l.Operand, v) Then
                        Dim a As AgcAddress = AgcCodec.PseudoToAddress(v)
                        desc &= " value=" & v.ToString() & " -> " & a.ToString()
                    Else
                        desc &= " (operand not numeric)"
                    End If
                End If
                Console.WriteLine("PROBE " & n & ": [" & text.Replace(Chr(9), "<T>") & "] -> " & desc)
            Next
        End Sub

        ''' <summary>
        ''' Print the raw field split of a handful of synthetic lines, so that the
        ''' column rules can be checked directly.
        ''' </summary>
        Public Sub DumpSynthetic()
            Dim t As String = Chr(9)
            Dim samples As String() = New String() { _
                "SETLOC" & t & "4000", _
                t & t & "SETLOC" & t & "4000", _
                "SETLOC" & t & t & "4000", _
                "LOOP" & t & "INCR" & t & "COUNT", _
                t & "CA" & t & "ONE", _
                t & t & "CA" & t & "ONE"}
            For Each s As String In samples
                Dim l As AgcLine = AgcSourceReader.ParseLine(s, "synthetic", 1)
                Dim desc As String
                If l Is Nothing Then
                    desc = "<null>"
                Else
                    desc = "label=[" & l.Label & "] op=[" & l.Opcode & "] operand=[" & l.Operand & "]"
                End If
                Console.WriteLine("SYN [" & s.Replace(t, "<T>") & "] -> " & desc)
            Next
        End Sub

    End Module

End Namespace
