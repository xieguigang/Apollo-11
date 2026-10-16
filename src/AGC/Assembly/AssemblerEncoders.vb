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
                    ' Two words: the sub-address, then the bank word.
                    Dim w0 As Integer = a.SReg And AgcWord.Mask15
                    Dim w1 As Integer = ((BankFieldValue(a) And &H3F) << 10) Or ((SuperBankNibbleFor(a) And 7) << 4)
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

        ''' <summary>
        ''' The 6-bit bank field of a bank word (BBCON / 2CADR) for a fixed address.
        ''' The field is the bank number the hardware sees in FBANK: the AGC bank
        ''' number, except that the sources number the super-bank module 030..043
        ''' while the hardware (and the ROR SUPERBNK dance the flight software uses
        ''' to build these words) numbers it 024..035 -- so eight is added.
        ''' </summary>
        Private Shared Function BankFieldValue(a As AgcAddress) As Integer
            Dim bank As Integer
            If Not a.Banked Then
                Dim agcBank As Integer = -1
                Dim offset As Integer = -1
                ResolveFixed(a, agcBank, offset)
                bank = If(agcBank > 0, agcBank, 2)
            ElseIf a.Super Then
                bank = a.FB + 8
            Else
                bank = a.FB
            End If
            Return bank And &H3F
        End Function

        ''' <summary>
        ''' The super-bank selector nibble for a bank word: 4 for the super-bank
        ''' module (banks 030..043), otherwise whatever SBANK= last established.
        ''' </summary>
        Private Function SuperBankNibbleFor(a As AgcAddress) As Integer
            Dim bank As Integer = BankFieldValue(a)
            If bank >= &H18 AndAlso bank <= &H1F Then Return If(a.Super, 4, 3)
            Return _sbank And 7
        End Function

        ''' <summary>BBCON / BBCON* -- a bank word: superbank in bits 4-6, F-bank in bits 10-15, E-bank in bits 0-2.</summary>
        Private Sub EmitBbcon(line As AgcLine, a As AgcAddress, star As Boolean)
            Dim target As AgcAddress = a
            Dim bankNumber As Integer
            Dim starBankField As Integer = -1

            If star Then
                ' BBCON* uses the last address of the last used fixed bank.  The
                ' per-bank counters follow the ".binsource" numbering, so the owner
                ' of the topmost non-empty slot gives the bank and the half.
                Dim bin As Integer = &H23
                Dim ownerBank As Integer = 0
                Dim halfOffset As Integer = 0
                Do While bin > 0
                    If BankUsed(bin) > 0 Then
                        BinSlotOwner(bin, ownerBank, halfOffset)
                        Exit Do
                    End If
                    bin -= 1
                Loop
                target.Kind = AgcMemoryKind.Fixed
                target.Banked = True
                target.Invalid = False
                target.Super = (ownerBank >= 24)
                target.FB = ownerBank
                target.SReg = &H800 + halfOffset + BankUsed(bin)
                bankNumber = ownerBank
                starBankField = ownerBank
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
                bankNumber = BankFieldValue(target)
            End If

            Dim value As Integer = (bankNumber And &H3F) << 10
            value = value Or (_ebank And 7)

            If star Then
                value = value Or ((starBankField And 7) << 4)
            Else
                value = value Or ((SuperBankNibbleFor(target) And 7) << 4)
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

            If a.IsFixed Then
                w1 = w1 Or ((BankFieldValue(a) And &H3F) << 10)
                w1 = w1 Or ((SuperBankNibbleFor(a) And 7) << 4)
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
