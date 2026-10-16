Imports System
Module Program
    Sub Main()
        Dim M As Integer = &H4000
        For Each v In {3, -3, 1, -1, 16383, -16383}
            Dim mag = Math.Abs(CLng(v))
            Dim hi = CInt((mag >> 14) And &H3FFF)
            Dim lo = CInt(mag And &H3FFF)
            If v < 0 Then
                hi = (M - hi) And &H3FFF
                lo = (M - lo) And &H3FFF
            End If
            Console.WriteLine("v=" & v.ToString().PadLeft(7) & " hi=0x" & hi.ToString("X4") &
                              " lo=0x" & lo.ToString("X4") & " signBit=" & ((hi And &H2000) <> 0).ToString())
        Next
    End Sub
End Module
