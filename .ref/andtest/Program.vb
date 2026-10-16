Imports System
Module Program
    Sub Main()
        Dim M As Integer = &H4000
        For Each v In {-3, -1, 3, 1}
            Dim mag As Long = Math.Abs(CLng(v))
            Dim hi As Integer = CInt((mag >> 14) And &H3FFF)
            Dim lo As Integer = CInt(mag And &H3FFF)
            If v < 0 Then
                hi = (hi + M) And &H3FFF
                lo = (lo + M) And &H3FFF
            End If
            ' read back
            Dim neg As Boolean = (hi And &H2000) <> 0
            Dim rhi As Integer = If(neg, (M - 1) - hi, hi) And &H3FFF
            Dim rlo As Integer = If(neg, (M - 1) - lo, lo) And &H3FFF
            Dim back As Integer = (rhi << 14) Or rlo
            If neg Then back = -back
            Console.WriteLine("v=" & v.ToString().PadLeft(4) & " hi=0x" & hi.ToString("X4") &
                              " lo=0x" & lo.ToString("X4") & " neg=" & neg.ToString() &
                              " readBack=" & back.ToString())
        Next
    End Sub
End Module
