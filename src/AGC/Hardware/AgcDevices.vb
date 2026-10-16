' ============================================================================
'  AgcDevices.vb -- the peripheral devices attached to the AGC's I/O channels:
'  the DSKY (display/keyboard), the IMU CDU counters, the downlink telemetry
'  stream and the simple discrete inputs.
' ============================================================================

Imports System
Imports System.Collections.Generic
Imports System.Text

Namespace Agc

    ''' <summary>The three CDU counter axes plus the optics shaft/trunnion.</summary>
    Public Enum CduChan
        X = 0
        Y = 1
        Z = 2
        S = 3
        T = 4
    End Enum

    ' ========================================================================
    ' DSKY
    ' ========================================================================

    Public Class AgcDsky

        Public Const NumDigits As Integer = 21

        ''' <summary>The 21 display digits, as rendered (0-9, blank, or a sign).</summary>
        Public ReadOnly Digits(NumDigits - 1) As Char

        ''' <summary>Set when the digits have been cleared because of a RESET.</summary>
        Public Property RestartLight As Boolean
        Public Property StbyLight As Boolean
        Public Property TempLight As Boolean
        Public Property KeyReleaseLight As Boolean
        Public Property OperatorErrorLight As Boolean
        Public Property CompActyLight As Boolean
        Public Property UplinkActyLight As Boolean
        Public Property NoAttLight As Boolean
        Public Property GimbalLockLight As Boolean
        Public Property ProgLight As Boolean
        Public Property TrackerLight As Boolean
        Public Property AltLight As Boolean
        Public Property VelLight As Boolean

        ''' <summary>The most recent key code pressed, in DSKY 5-bit row/column form.</summary>
        Public Property PendingKey As Integer = -1
        ''' <summary>The value of the PRO word (channel 016).</summary>
        Public Property ProWord As Integer = AgcWord.Mask15
        ''' <summary>The value of the RESTART word (channel 017).</summary>
        Public Property RsetWord As Integer = AgcWord.Mask15

        ''' <summary>Current verb / noun as set by the display routines.</summary>
        Public Property Verb As Integer
        Public Property Noun As Integer

        ''' <summary>Textual log of display updates (for the simulation harness).</summary>
        Public ReadOnly DisplayLog As New List(Of String)()

        Private ReadOnly _channel011 As Integer
        Private ReadOnly _channel013 As Integer

        Public Sub New()
            Reset()
        End Sub

        Public Sub Reset()
            For i As Integer = 0 To NumDigits - 1
                Digits(i) = " "c
            Next
            RestartLight = True
            StbyLight = False
            TempLight = False
            KeyReleaseLight = False
            OperatorErrorLight = False
            CompActyLight = False
            UplinkActyLight = False
            NoAttLight = False
            GimbalLockLight = False
            ProgLight = False
            TrackerLight = False
            AltLight = False
            VelLight = False
            PendingKey = -1
            ProWord = AgcWord.Mask15
            RsetWord = AgcWord.Mask15
            Verb = 0
            Noun = 0
            DisplayLog.Clear()
        End Sub

        ' ------------------------------------------------------------------
        ' Output: channel 010 relay rows
        ' ------------------------------------------------------------------

        ''' <summary>
        ''' Channel 010 selects a relay row with its high bits; the low 11 bits
        ''' are the row data.
        ''' </summary>
        Public Sub WriteRelayRow(row As Integer, value As Integer)
            ' Relay rows 0-10 are the DSKY row selectors, plus the "sign" rows
            ' and the relay-row latches used by the display interface routines.
            ' The DSKY digit decoding is performed by the AGC software itself
            ' (which writes the row number into the high bits of channel 010);
            ' we record enough information for the host to reconstruct what is
            ' on the display.
            If row <= 10 Then
                ' Row selects one of the 11 digit positions and the low 5 bits
                ' hold the character.
                Dim position As Integer = row
                If position = 10 Then
                    ' Row 11 (relay row 11) holds the sign of the first
                    ' register plus the PROG/VERB/NOUN indicators.
                Else
                    Dim code As Integer = value And &H1F
                    Digits(position) = DecodeDigit(code)
                End If
            End If
        End Sub

        ''' <summary>Decode a 5-bit DSKY digit code (the AGC's "binary coded octal").</summary>
        Public Shared Function DecodeDigit(code As Integer) As Char
            ' The AGC drives the display with a 5-bit code: bit 4 is the sign
            ' bit (segment), bits 1-3 are the binary digit, bit 0 dumps to zero.
            Dim v As Integer = code And &HF
            If v > 9 Then Return " "c
            Return ChrW(AscW("0"c) + v)
        End Function

        Public Sub WriteChannel011(value As Integer)
            ' Channel 011 bit 12 selects the display bank, bit 11 the "sign"
            ' mode; the DSKY also latches the PROG/VERB/NOUN selectors here.
            If (value And &H400) <> 0 Then
                ' PROG/VERB/NOUN display selected.
            End If
        End Sub

        Public Sub WriteChannel013(value As Integer)
            ' Channel 013 controls the DSKY warning lights and the external
            ' "flash" / telescope enable discretes.
            CompActyLight = (value And &H2) <> 0
            UplinkActyLight = (value And &H4) <> 0
            NoAttLight = (value And &H8) <> 0
            StbyLight = (value And &H10) <> 0
            KeyReleaseLight = (value And &H20) <> 0
            OperatorErrorLight = (value And &H40) <> 0
            GimbalLockLight = (value And &H80) <> 0
            TrackerLight = (value And &H100) <> 0
            AltLight = (value And &H1000) <> 0
            VelLight = (value And &H2000) <> 0
        End Sub

        Public Sub WriteChannel014(value As Integer)
            ' Channel 014 controls the R1/R2/R3 and COMP ACTY "flash" latches.
        End Sub

        ' ------------------------------------------------------------------
        ' Input: key presses
        ' ------------------------------------------------------------------

        ''' <summary>
        ''' Press a DSKY key.  <paramref name="row"/> is the relay row (1-5) and
        ''' <paramref name="column"/> the key column (1-5) of the keypad matrix;
        ''' use the Key* constants or supply raw values.
        ''' </summary>
        Public Sub PressKey(row As Integer, column As Integer)
            ' Channel 015 carries the key code in the low bits plus the PRO
            ' and key-release discretes in the high bits.
            PendingKey = ((row And 7) << 3) Or (column And 7)
            ProWord = AgcWord.Mask15 And Not (1 << (PendingKey And &HF))
        End Sub

        Public Sub ReleaseKey()
            PendingKey = -1
            ProWord = AgcWord.Mask15
        End Sub

        ''' <summary>The key code the software will read from channel 015.</summary>
        Public Function ReadKeys() As Integer
            If PendingKey < 0 Then Return AgcWord.Mask15
            ' The AGC expects the complement of the pressed-key bit pattern.
            Return (AgcWord.Mask15 And Not (1 << (PendingKey And &HF))) And AgcWord.Mask15
        End Function

        Public Sub PressPro()
            ProWord = ProWord And Not &H20
        End Sub

        Public Sub ReleasePro()
            ProWord = ProWord Or &H20
        End Sub

        Public Sub PressReset()
            RsetWord = RsetWord And Not &H20
        End Sub

        Public Sub ReleaseReset()
            RsetWord = RsetWord Or &H20
        End Sub

        ' ------------------------------------------------------------------
        ' Rendering
        ' ------------------------------------------------------------------

        ''' <summary>Render the current display as three lines, DSKY style.</summary>
        Public Function Render() As String
            Dim sb As New StringBuilder()
            sb.AppendLine("+------+------+------+")
            sb.Append("|PROG  |VERB  |NOUN  |").AppendLine()
            sb.AppendLine("+------+------+------+")
            sb.AppendLine(" R1/R2/R3:")
            sb.Append("  ").Append(CharAt(0)).Append(CharAt(1)).Append(CharAt(2)).Append(CharAt(3)).Append(CharAt(4))
            sb.AppendLine()
            sb.Append("  ").Append(CharAt(5)).Append(CharAt(6)).Append(CharAt(7)).Append(CharAt(8)).Append(CharAt(9))
            sb.AppendLine()
            sb.Append("  ").Append(CharAt(10)).Append(CharAt(11)).Append(CharAt(12)).Append(CharAt(13)).Append(CharAt(14))
            sb.AppendLine()
            Return sb.ToString()
        End Function

        Private Function CharAt(index As Integer) As Char
            If index < 0 OrElse index >= NumDigits Then Return " "c
            Return Digits(index)
        End Function

    End Class

    ' ========================================================================
    ' IMU CDU counters
    ' ========================================================================

    ''' <summary>
    ''' The five CDU counters (X, Y, Z, optics shaft and trunnion).  Each is a
    ''' 15-bit register that the software reads through the channels and that the
    ''' 1/3200 second "counter" commands drive.
    ''' </summary>
    Public Class AgcCdu

        ''' <summary>Counter values in the AGC's 2's-complement 15-bit format.</summary>
        Public ReadOnly Counters(4) As Integer

        ''' <summary>Number of times each counter overflowed.</summary>
        Public ReadOnly Overflows(4) As Integer

        Public Sub New()
            Reset()
        End Sub

        Public Sub Reset()
            Array.Clear(Counters, 0, Counters.Length)
            Array.Clear(Overflows, 0, Overflows.Length)
        End Sub

        ''' <summary>
        ''' Apply a CDU increment command.  <paramref name="opcode"/> selects the
        ''' pulse style: 0 = positive pulse, 1 = negative pulse, 2 = positive
        ''' pulse x2, 3 = negative pulse x2, 4 = shift left, 5 = shift left +1.
        ''' </summary>
        Public Sub IncrementChannel(channel As CduChan, opcode As Integer)
            Dim i As Integer = CInt(channel)
            Select Case opcode And 7
                Case 0
                    PcdIncrement(i)
                Case 1
                    PcdDecrement(i)
                Case 2
                    PcdIncrement(i) : PcdIncrement(i)
                Case 3
                    PcdDecrement(i) : PcdDecrement(i)
                Case 4
                    ShiftLeft(i, False)
                Case 5
                    ShiftLeft(i, True)
                Case 6
                    ' "Shift right" style: not used by the flight software.
                    ShiftRight(i)
                Case Else
                    PcdIncrement(i)
            End Select
        End Sub

        Public Sub DecrementChannel(channel As CduChan)
            PcdDecrement(CInt(channel))
        End Sub

        ''' <summary>Set a counter to a physical angle in units of 2^-15 turns.</summary>
        Public Sub SetCounter(channel As CduChan, value As Integer)
            Counters(CInt(channel)) = value And AgcWord.Mask15
        End Sub

        Public Function GetCounter(channel As CduChan) As Integer
            Return Counters(CInt(channel))
        End Function

        ''' <summary>2's-complement increment (PCDU-style pulse).</summary>
        Private Sub PcdIncrement(i As Integer)
            Dim v As Integer = Counters(i)
            If v = &H7FFF Then Overflows(i) += 1
            v = (v + 1) And &H7FFF
            Counters(i) = v
        End Sub

        Private Sub PcdDecrement(i As Integer)
            Dim v As Integer = Counters(i)
            If v = 0 Then Overflows(i) += 1
            v = (v - 1) And &H7FFF
            Counters(i) = v
        End Sub

        Private Sub ShiftLeft(i As Integer, addOne As Boolean)
            Dim v As Integer = Counters(i)
            If (v And &H2000) <> 0 Then Overflows(i) += 1
            v = ((v << 1) Or If(addOne, 1, 0)) And &H3FFF
            Counters(i) = v
        End Sub

        Private Sub ShiftRight(i As Integer)
            Dim v As Integer = Counters(i)
            Counters(i) = (v >> 1) And &H3FFF
        End Sub

    End Class

    ' ========================================================================
    ' Downlink telemetry
    ' ========================================================================

    ''' <summary>
    ''' Collects the words written to channels 034/035, i.e. the AGC's downlink
    ''' telemetry stream.  A host program can decode these with the AGC's
    ''' downlink lists; here we simply record them.
    ''' </summary>
    Public Class AgcDownlink

        Public ReadOnly Words As New List(Of Integer)()
        Public Property Enabled As Boolean = True
        Public Property MaxWords As Integer = 200000

        Public Sub Reset()
            Words.Clear()
        End Sub

        Public Sub SendTelemetry(value As Integer)
            If Not Enabled Then Return
            If Words.Count >= MaxWords Then Return
            Words.Add(value And AgcWord.Mask15)
        End Sub

    End Class

End Namespace
