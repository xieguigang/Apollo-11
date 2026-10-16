' ============================================================================
'  AgcWord.vb -- 15-bit AGC word arithmetic (one's complement), octal parsing
'  and the decimal/scaled-number conversions used by the AGC assembler.
'
'  AGC numbers
'  -----------
'  Every value that lives in memory is a 15-bit one's-complement word:
'
'      +0 = 00000 (octal)      -0 = 77777      -1 = 77776      -2 = 77775
'
'  Arithmetic in this module is done on plain Integer values with the low 15
'  bits significant.  The AGC's adder uses an end-around carry: the carry out
'  of bit 15 is added back into bit 1.  Adding two minus zeros yields plus
'  zero, which the hardware achieves because the all-ones pattern plus the
'  end-around carry wraps round to zero.
' ============================================================================

Imports System
Imports System.Globalization
Imports System.Runtime.CompilerServices

Namespace Agc

    Public Module AgcWord

        Public Const Bits As Integer = 15
        Public Const Mask15 As Integer = 32767          ' 077777
        Public Const Mask12 As Integer = 4095           ' 07777
        Public Const Mask11 As Integer = 2047           ' 03777
        Public Const Mask10 As Integer = 1023           ' 01777
        Public Const Mask9 As Integer = 511             ' 0777
        Public Const Mask8 As Integer = 255             ' 0377
        Public Const SignBit As Integer = 16384         ' 020000
        Public Const CarryBit As Integer = 32768        ' 040000

        ' ------------------------------------------------------------------
        ' Normalisation and conversion
        ' ------------------------------------------------------------------

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Function Normalize(value As Integer) As Integer
            Return value And Mask15
        End Function

        ''' <summary>Signed interpretation of a 15-bit one's-complement word.</summary>
        Public Function ToSigned(word As Integer) As Integer
            Dim w As Integer = word And Mask15
            If w >= SignBit Then Return -(w Xor Mask15)
            Return w
        End Function

        ''' <summary>Encode a signed integer as a 15-bit one's-complement word.</summary>
        Public Function FromSigned(value As Integer) As Integer
            If value < 0 Then Return (Mask15 + value) And Mask15
            Return value And Mask15
        End Function

        ' ------------------------------------------------------------------
        ' One's-complement arithmetic
        ' ------------------------------------------------------------------

        ''' <summary>
        ''' One's-complement addition with the AGC's end-around carry.  When the
        ''' 16-bit sum exceeds 15 bits the carry is added back into bit 1, which
        ''' leaves the low 15 bits alone; a "-0 + -0" pair therefore becomes
        ''' plus zero, exactly as the hardware behaves.
        ''' </summary>
        Public Function Add(a As Integer, b As Integer) As Integer
            Dim s As Integer = (a And Mask15) + (b And Mask15)
            If s > Mask15 Then s = s - CarryBit
            s = s And Mask15
            If s = Mask15 Then s = 0
            Return s
        End Function

        ''' <summary>
        ''' One's-complement addition that preserves a result of all ones (minus
        ''' zero).  Used by the negation helpers.
        ''' </summary>
        Public Function AddPreservingZero(a As Integer, b As Integer) As Integer
            Dim s As Integer = (a And Mask15) + (b And Mask15)
            If s > Mask15 Then s = s - CarryBit
            Return s And Mask15
        End Function

        ''' <summary>One's complement (bitwise NOT within 15 bits).</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Function Complement(a As Integer) As Integer
            Dim x As Integer = a And Mask15
            Return Mask15 - x
        End Function

        ''' <summary>Arithmetic negation (complement plus one); may yield minus zero.</summary>
        Public Function Negate(a As Integer) As Integer
            Dim x As Integer = a And Mask15
            Dim c As Integer = Mask15 - x
            Dim s As Integer = c + 1
            If s > Mask15 Then s = s - CarryBit
            Return s And Mask15
        End Function

        ''' <summary>Absolute value of a signed AGC word; minus zero counts as zero.</summary>
        Public Function AbsWord(a As Integer) As Integer
            Dim x As Integer = a And Mask15
            If x = Mask15 Then Return 0
            If x >= SignBit Then Return Negate(x)
            Return x
        End Function

        Public Function ToOctal(word As Integer, Optional digits As Integer = 5) As String
            Return Convert.ToString(word And Mask15, 8).PadLeft(digits, "0"c)
        End Function

        ' ------------------------------------------------------------------
        ' Bit helpers
        '
        ' The AGC sources are full of bit-field manipulations.  These helpers
        ' express them arithmetically, which keeps the intent obvious and avoids
        ' relying on the sign behaviour of bitwise operators.
        ' ------------------------------------------------------------------

        ''' <summary>True when bit <paramref name="bit"/> (0-based) is set.</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Function HasBit(value As Integer, bit As Integer) As Boolean
            Return ((value >> bit) Mod 2) <> 0
        End Function

        ''' <summary>Set or clear bit <paramref name="bit"/>.</summary>
        Public Function SetBit(value As Integer, bit As Integer, enabled As Boolean) As Integer
            Dim power As Integer = 1 << bit
            If enabled Then
                If (value >> bit) Mod 2 = 0 Then Return value + power
                Return value
            End If
            If (value >> bit) Mod 2 <> 0 Then Return value - power
            Return value
        End Function

        ''' <summary>Bitwise OR, computed arithmetically.</summary>
        Public Function OrBits(a As Integer, b As Integer) As Integer
            Dim result As Integer = 0
            Dim power As Integer = 1
            Do While power <> 0 AndAlso power <= a + b
                If ((a \ power) Mod 2) <> 0 OrElse ((b \ power) Mod 2) <> 0 Then result += power
                power = power << 1
            Loop
            Return result
        End Function

        ''' <summary>Bitwise AND, computed arithmetically.</summary>
        Public Function AndBits(a As Integer, b As Integer) As Integer
            Dim result As Integer = 0
            Dim power As Integer = 1
            Do While power <> 0 AndAlso power <= a AndAlso power <= b
                If ((a \ power) Mod 2) <> 0 AndAlso ((b \ power) Mod 2) <> 0 Then result += power
                power = power << 1
            Loop
            Return result
        End Function

        ''' <summary>Bitwise XOR, computed arithmetically.</summary>
        Public Function XorBits(a As Integer, b As Integer) As Integer
            Dim result As Integer = 0
            Dim power As Integer = 1
            Do While power <> 0 AndAlso power <= a + b
                Dim ba As Integer = (a \ power) Mod 2
                Dim bb As Integer = (b \ power) Mod 2
                If (ba <> 0) <> (bb <> 0) Then result += power
                power = power << 1
            Loop
            Return result
        End Function

        ' ------------------------------------------------------------------
        ' Numeric literal parsing
        ' ------------------------------------------------------------------

        ''' <summary>
        ''' Parse an integer literal exactly the way the AGC assembler does
        ''' (GetOctOrDec): a token made only of digits 0-7 is OCTAL -- so "4000" is
        ''' 04000, not four thousand -- while a token containing an 8 or a 9, or
        ''' ending in "D", is decimal.  "&amp;O" forces octal.
        ''' </summary>
        Public Function TryParseInteger(text As String, ByRef value As Integer) As Boolean
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
            If i >= s.Length Then Return False

            ' An explicit "&O" prefix forces octal.
            Dim forceOctal As Boolean = False
            If i + 1 < s.Length AndAlso s(i) = "&"c AndAlso Char.ToUpperInvariant(s(i + 1)) = "O"c Then
                forceOctal = True
                i += 2
            End If

            Dim body As New Text.StringBuilder()
            Dim has8or9 As Boolean = False
            While i < s.Length
                Dim c As Char = s(i)
                If c >= "0"c AndAlso c <= "9"c Then
                    If c = "8"c OrElse c = "9"c Then has8or9 = True
                    body.Append(c)
                    i += 1
                Else
                    Exit While
                End If
            End While
            If body.Length = 0 Then Return False

            Dim tail As String = s.Substring(i).ToUpperInvariant()
            Dim isDecimal As Boolean
            If forceOctal Then
                isDecimal = False
                If tail <> "" Then Return False
            ElseIf tail = "D" Then
                isDecimal = True
            ElseIf tail = "" Then
                isDecimal = has8or9
            Else
                Return False
            End If

            Dim raw As Long
            If isDecimal Then
                If Not Long.TryParse(body.ToString(), NumberStyles.None, CultureInfo.InvariantCulture, raw) Then Return False
            Else
                raw = 0L
                For Each c As Char In body.ToString()
                    If c < "0"c OrElse c > "7"c Then Return False
                    raw = raw * 8L + CLng(AscW(c) - AscW("0"c))
                Next
            End If

            value = CInt(If(negative, -raw, raw))
            Return True
        End Function

        Public Function IsIntegerLiteral(text As String) As Boolean
            Dim dummy As Integer
            Return TryParseInteger(text, dummy)
        End Function

        ' ------------------------------------------------------------------
        ' Decimal constants with optional scale factors
        ' ------------------------------------------------------------------

        ''' <summary>
        ''' The scale factor of a trailing "Enn" (power of ten) or "Bnn" (power of
        ''' two) modifier, as used by the DEC and 2DEC pseudo-ops.
        ''' </summary>
        Public Function ScaleFactor(s As String) As Double
            If String.IsNullOrEmpty(s) Then Return 1.0
            Dim marker As Char = Char.ToUpperInvariant(s(0))
            If marker <> "E"c AndAlso marker <> "B"c Then Return 1.0
            Dim n As Integer
            If Not Integer.TryParse(s.Substring(1), NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture, n) Then Return 1.0
            If marker = "E"c Then Return Math.Pow(10.0, n)
            Return Math.Pow(2.0, n)
        End Function

        Private Const Fuzz As Double = 1.0E-10

        Private Function FuzzedIsInt(x As Double) As Boolean
            Dim f As Double = Math.Abs(x - Math.Truncate(x))
            Return f < Fuzz OrElse f > 1.0 - Fuzz
        End Function

        ''' <summary>
        ''' Convert a signed double to a one's-complement binary fraction of the
        ''' requested width (14 bits for single precision, 28 for double).
        ''' </summary>
        Private Function ToBinaryFraction(x As Double, bits As Integer, ByRef ok As Boolean) As Long
            ok = True
            Dim mask As Long = (1L << bits) - 1L
            Dim negative As Boolean = x < 0.0
            If negative Then x = -x

            Dim value As Long
            If FuzzedIsInt(x) Then
                ' An integer constant is used directly.  Values that do not fit
                ' the field are truncated (with a warning), as yaYUL does.
                If x > CDbl(mask) Then
                    ok = False
                    value = mask
                Else
                    value = CLng(Math.Truncate(x))
                End If
                value = value And mask
            Else
                If x >= 1.0 Then
                    ok = False
                    Return 0L
                End If
                value = 0L
                For i As Integer = 0 To bits - 1
                    value = (value << 1) And mask
                    If x >= 0.5 Then
                        value += 1L
                        x -= 0.5
                    End If
                    x *= 2.0
                Next
                If x >= 0.5 AndAlso value < mask Then value += 1L
            End If
            If negative Then value = value Xor mask
            Return value And mask
        End Function

        ''' <summary>
        ''' DEC pseudo-op: a single-precision (14-bit) constant.  The default scale
        ''' is B-14 when the operand has no decimal point and no explicit scale.
        ''' </summary>
        Public Function EncodeSinglePrecision(value As Double, scale1 As String, scale2 As String,
                                               hasDecimalPoint As Boolean, ByRef ok As Boolean) As Integer
            Dim s1 As String = scale1
            If Not hasDecimalPoint AndAlso String.IsNullOrEmpty(scale1) AndAlso String.IsNullOrEmpty(scale2) Then
                s1 = "B-14"
            End If
            Dim x As Double = value * ScaleFactor(s1) * ScaleFactor(scale2)
            Dim v As Long = ToBinaryFraction(x, 14, ok)
            If Not ok Then Return 0
            Return CInt(v And Mask15)
        End Function

        ''' <summary>
        ''' 2DEC pseudo-op: a double-precision (28-bit) constant, returned as the
        ''' high and low 15-bit words.
        ''' </summary>
        Public Sub EncodeDoublePrecision(value As Double, scale1 As String, scale2 As String,
                                         hasDecimalPoint As Boolean,
                                         ByRef high As Integer, ByRef low As Integer, ByRef ok As Boolean)
            Dim s1 As String = scale1
            If Not hasDecimalPoint AndAlso String.IsNullOrEmpty(scale1) AndAlso String.IsNullOrEmpty(scale2) Then
                s1 = "B-28"
            End If
            Dim x As Double = value * ScaleFactor(s1) * ScaleFactor(scale2)
            Dim v As Long = ToBinaryFraction(x, 28, ok)
            If Not ok Then
                high = 0 : low = 0
                Return
            End If
            low = CInt(v And &H3FFFL)
            high = CInt((v >> 14) And &H3FFFL)
        End Sub

        ''' <summary>
        ''' VN/NV pseudo-op: pack a two-group decimal such as "06 07" into a single
        ''' word; the upper group is multiplied by 128.
        ''' </summary>
        Public Function EncodeVerbNoun(text As String, ByRef ok As Boolean) As Integer
            ok = False
            Dim digits As New Text.StringBuilder()
            For Each c As Char In text
                If c >= "0"c AndAlso c <= "9"c Then digits.Append(c)
            Next
            If digits.Length = 0 Then Return 0
            Dim v As Integer
            If Not Integer.TryParse(digits.ToString(), NumberStyles.None, CultureInfo.InvariantCulture, v) Then Return 0
            ok = True
            Return ((v Mod 100) Or ((v \ 100) << 7)) And Mask15
        End Function

    End Module

End Namespace
