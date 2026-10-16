' ============================================================================
'  AssemblerEncoders.vb -- address/data word encoders for the AGC assembler.
'
'  These reproduce the pseudo-ops that place an address into a data word:
'  GENADR/ADRES, CADR/FCADR, ECADR, BBCON, 2CADR, the downlink telemetry
'  pseudo-ops (nDNADR, DNCHAN, DNPTR) and the XCADR form.
' ============================================================================

Imports System
Imports System.Collections.Generic

Namespace Agc

    Partial Public Class AgcAssembler

        Private Enum AddressFormat
            GenericSubAddress
            Cadr
            TwoFcadr
            Ecadr
            Bbcon
            BbconStar
            TwoCadr
            DownlinkPointer
            Xcadr
        End Enum

        ' ------------------------------------------------------------------
        ' Address pseudo-ops
        ' ------------------------------------------------------------------

        Private Sub EmitAddress(line As AgcLine, format As AddressFormat)
            Dim name As String = line.Operand
            Dim off As Integer = 0
            Dim numeric As Boolean = False
            Dim a As AgcAddress = ResolveOperand(name, off, numeric)

            If a.Invalid Then
                Diagnostics.Fatal(line.File, line.LineNumber,
                                  "Symbol '" & line.Operand & "' undefined or offset bad.")
                EmitAndAdvance(WordCountFor(format))
                Return
            End If

            Select Case format

                Case AddressFormat.GenericSubAddress
                    If Not a.IsAddress Then
                        Diagnostics.Fatal(line.File, line.LineNumber, "Destination is not a memory address.")
                        EmitAndAdvance(New Integer() {0})
                        Return
                    End If
                    EmitAndAdvance(New Integer() {a.SReg And AgcWord.Mask15})

                Case AddressFormat.Cadr
                    If Not a.IsFixed OrElse Not a.Banked Then
                        Diagnostics.Fatal(line.File, line.LineNumber, "Destination not in an F-bank.")
                        EmitAndAdvance(New Integer() {0})
                        Return
                    End If
                    Dim v As Integer = a.CadrValue
                    If v < 0 OrElse v > &HFFF Then
                        Diagnostics.Fatal(line.File, line.LineNumber, "Destination address out of range.")
                        v = 0
                    End If
                    EmitAndAdvance(New Integer() {v})

                Case AddressFormat.TwoFcadr
                    If Not a.IsFixed Then
                        Diagnostics.Fatal(line.File, line.LineNumber, "Destination not in fixed memory.")
                        EmitAndAdvance(New Integer() {0, 0})
                        Return
                    End If
                    Dim w0 As Integer, w1 As Integer
                    If a.SReg >= &H800 AndAlso a.SReg <= &HFFF Then
                        w0 = ((a.FB << 10) Or (a.SReg - &H800)) And AgcWord.Mask15
                        w1 = a.SReg And AgcWord.Mask15
                    ElseIf a.SReg >= &H1000 AndAlso a.SReg <= &H1FFF Then
                        w0 = a.SReg And AgcWord.Mask15
                        w1 = a.SReg And AgcWord.Mask15
                    Else
                        Diagnostics.Fatal(line.File, line.LineNumber, "Internal error implementing 2FCADR.")
                        w0 = 0 : w1 = 0
                    End If
                    EmitAndAdvance(New Integer() {w0, w1})

                Case AddressFormat.Ecadr
                    If Not a.IsErasable Then
                        Diagnostics.Fatal(line.File, line.LineNumber, "Destination not in erasable memory.")
                        EmitAndAdvance(New Integer() {0})
                        Return
                    End If
                    EmitAndAdvance(New Integer() {EcadrOf(a)})

                Case AddressFormat.Bbcon, AddressFormat.BbconStar
                    EmitBbcon(line, a, format = AddressFormat.BbconStar)

                Case AddressFormat.TwoCadr
                    EmitTwoCadr(line, a)

                Case AddressFormat.DownlinkPointer
                    EmitDownlinkPointer(line, a)

                Case AddressFormat.Xcadr
                    EmitAndAdvance(New Integer() {(&H27FF + a.SReg) And AgcWord.Mask15})
            End Select
        End Sub

        Private Shared Function WordCountFor(format As AddressFormat) As Integer()
            Select Case format
                Case AddressFormat.TwoFcadr, AddressFormat.TwoCadr
                    Return New Integer() {0, 0}
                Case Else
                    Return New Integer() {0}
            End Select
        End Function

        ''' <summary>BBCON / BBCON* -- a bank word: superbank in bits 4-6, F-bank in bits 10-14, E-bank in bits 0-2.</summary>
        Private Sub EmitBbcon(line As AgcLine, a As AgcAddress, star As Boolean)
            Dim target As AgcAddress = a
            Dim bankNumber As Integer

            If star Then
                ' BBCON* uses the last address of the last used fixed bank.
                Dim b As Integer = &H23
                Do While b > 0 AndAlso BankUsed(b) = 0
                    b -= 1
                Loop
                target.Kind = AgcMemoryKind.Fixed
                target.Banked = True
                target.Invalid = False
                target.Super = (b >= &H20)
                target.FB = If(b >= &H20, b - 8, b)
                target.SReg = &H1000 + BankUsed(b)
                bankNumber = b
            Else
                If Not target.IsFixed Then
                    Diagnostics.Fatal(line.File, line.LineNumber, "Destination is not in fixed memory.")
                    EmitAndAdvance(New Integer() {0})
                    Return
                End If
                Dim sreg As Integer = target.SReg And AgcWord.Mask15
                If sreg < &H800 OrElse sreg > &H1FFF Then
                    Diagnostics.Fatal(line.File, line.LineNumber, "Destination address out of range.")
                    EmitAndAdvance(New Integer() {0})
                    Return
                End If
                bankNumber = If(target.Banked, target.FB, sreg \ &H800)
            End If

            Dim value As Integer = (bankNumber And &H1F) << 10
            value = value Or (_ebank And 7)

            Dim targetBank As Integer = If(target.Super, target.FB + 8, target.FB)
            If (Not star) AndAlso targetBank >= &H18 AndAlso targetBank <= &H1F Then
                value = value Or (If(target.Super, 4, 3) << 4)
            ElseIf Not star Then
                value = value Or ((_sbank And 7) << 4)
            End If

            EmitAndAdvance(New Integer() {value And AgcWord.Mask15})
        End Sub

        ''' <summary>2CADR -- a two word fixed-memory pointer (sub-address, then bank word).</summary>
        Private Sub EmitTwoCadr(line As AgcLine, a As AgcAddress)
            If Not a.IsAddress Then
                Diagnostics.Fatal(line.File, line.LineNumber, "Destination is not a memory address.")
                EmitAndAdvance(New Integer() {0, 0})
                Return
            End If

            Dim w0 As Integer = a.SReg And AgcWord.Mask15
            Dim w1 As Integer = _ebank And 7

            If a.SReg >= &H800 Then
                If a.SReg <= &HFFF Then
                    w1 = w1 Or ((a.FB And &H1F) << 10)
                Else
                    w1 = w1 Or (((a.SReg \ &H800) And &H1F) << 10)
                End If
            End If

            Dim targetBank As Integer = If(a.Super, a.FB + 8, a.FB)
            If targetBank >= &H18 AndAlso targetBank <= &H1F Then
                w1 = w1 Or (If(a.Super, 4, 3) << 4)
            Else
                w1 = w1 Or ((_sbank And 7) << 4)
            End If

            EmitAndAdvance(New Integer() {w0, w1})
        End Sub

        ''' <summary>
        ''' Downlink pointer pseudo-ops: the address is complemented and biased so
        ''' that the telemetry formatter can walk the list.
        ''' </summary>
        Private Sub EmitDownlinkPointer(line As AgcLine, a As AgcAddress)
            Dim base As Integer = &H1800   ' 030000: DNPTR/-DNPTR add this
            Dim raw As Integer = (a.SReg And AgcWord.Mask15) + base
            raw = raw And AgcWord.Mask15
            EmitAndAdvance(New Integer() {(AgcWord.Complement(raw) - 1) And AgcWord.Mask15})
        End Sub

        ''' <summary>nDNADR / -nDNADR: an erasable address with a 4-bit group index.</summary>
        Private Sub EmitDnaddr(line As AgcLine)
            Dim op As String = line.Opcode
            Dim negative As Boolean = op.StartsWith("-", StringComparison.Ordinal)
            Dim body As String = op.TrimStart("-"c)
            Dim group As Integer = 0
            If body.Length > 0 AndAlso Char.IsDigit(body(0)) Then
                group = AscW(body(0)) - AscW("0"c)
            End If
            Dim base As Integer = group * &H1000

            Dim name As String = line.Operand
            Dim off As Integer = 0
            Dim numeric As Boolean = False
            Dim a As AgcAddress = ResolveOperand(name, off, numeric)
            If a.Invalid Then
                Diagnostics.Fatal(line.File, line.LineNumber, "Symbol '" & line.Operand & "' undefined.")
                EmitAndAdvance(New Integer() {0})
                Return
            End If
            If Not a.IsErasable Then
                Diagnostics.Fatal(line.File, line.LineNumber, "Destination not in erasable memory.")
                EmitAndAdvance(New Integer() {0})
                Return
            End If

            Dim word As Integer = (EcadrOf(a) + base) And AgcWord.Mask15
            If negative Then word = (AgcWord.Complement(word) - 1) And AgcWord.Mask15
            EmitAndAdvance(New Integer() {word})
        End Sub

        ''' <summary>DNCHAN / -DNCHAN: encode an I/O channel number for the downlink list.</summary>
        Private Sub EmitDnchan(line As AgcLine)
            Dim negative As Boolean = line.Opcode.StartsWith("-", StringComparison.Ordinal)
            Dim name As String = line.Operand
            Dim off As Integer = 0
            Dim numeric As Boolean = False
            Dim a As AgcAddress = ResolveOperand(name, off, numeric)
            Dim channel As Integer
            If a.Invalid Then
                channel = 0
                Diagnostics.Fatal(line.File, line.LineNumber, "DNCHAN operand '" & line.Operand & "' undefined.")
            Else
                channel = a.SReg And &HFF
            End If
            Dim word As Integer = (&H1C00 Or channel) And AgcWord.Mask15
            If negative Then word = (AgcWord.Complement(word) - 1) And AgcWord.Mask15
            EmitAndAdvance(New Integer() {word})
        End Sub

    End Class

End Namespace
