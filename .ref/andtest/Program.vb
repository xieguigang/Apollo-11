Imports System
Module Program
    Sub Main()
        Dim w As Integer = Convert.ToInt32("77745", 8)
        Console.WriteLine("DLOAD word = " & w.ToString() & " hex=0x" & w.ToString("X"))
        Console.WriteLine("w >> 9 = " & (w >> 9).ToString() & "  (w>>9)&127 = " & ((w >> 9) And 127).ToString())
        Dim c As Integer = (32767 And Not w) And 127
        Console.WriteLine("low7 = " & c.ToString() & " -> code = " & (c - 1).ToString())
    End Sub
End Module
