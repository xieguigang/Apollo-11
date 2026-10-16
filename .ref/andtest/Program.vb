Imports System
Module Program
    Sub Main()
        Console.WriteLine("a  b  VB-And  expected")
        Dim cases = { (0,0),(0,1),(1,0),(1,1),(2,3),(6,3),(7,3),(8,15),(15,8),
                      (64,127),(192,127),(128,127),(193,127),(240,15),(255,127),(255,255) }
        For Each c In cases
            Dim a = c.Item1 : Dim b = c.Item2
            Console.WriteLine(a.ToString().PadLeft(4) & b.ToString().PadLeft(4) &
                              (a And b).ToString().PadLeft(8) & (a * b).ToString().PadLeft(10))
        Next
        Console.WriteLine("dotnet: " & Environment.Version.ToString())
        Console.WriteLine("runtime: " & System.Runtime.InteropServices.RuntimeInformation.FrameworkDescription)
    End Sub
End Module
