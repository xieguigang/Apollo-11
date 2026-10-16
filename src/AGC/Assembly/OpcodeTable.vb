' ============================================================================
'  OpcodeTable.vb -- the AGC Block II instruction set as understood by the
'  assembler.
'
'  Opcode values are the octal bit patterns that occupy bits 9-11 of a basic
'  instruction word (bits 9-14 when the EXTEND prefix is in effect).  They have
'  been verified against the yaAGC reference engine's instruction decode, which
'  is authoritative for the Block II hardware:
'
'      extended opcode = (instruction >> 9) | 0100 (octal) when EXTEND is set
'
'  so, for example, BZF = 010000, 010000 >> 9 = 010, OR 0100 = 0110 => BZF.
' ============================================================================

Imports System
Imports System.Collections.Generic

Namespace Agc

    ''' <summary>
    ''' Instruction properties the assembler uses when encoding an operand.
    ''' </summary>
    Public Enum OperandFlags As Integer
        None = 0
        ' <summary>Requires an EXTEND prefix (bits 9-14 hold the opcode).</summary>
        Extended = 1
        ' <summary>Operand must resolve to fixed memory.</summary>
        Fixed = 2
        ' <summary>Operand must resolve to erasable memory.</summary>
        Erasable = 4
        ' <summary>Store the operand address incremented by one.</summary>
        Plus1 = 8
        ' <summary>Numeric operand is legal.</summary>
        Number = 16
        ' <summary>Quarter code 0 (operand 0000..01777).</summary>
        QC0 = 32
        QC1 = 64
        QC2 = 128
        QC3 = 256
        ' <summary>Operand must have a non-zero quarter code (02000..07777).</summary>
        QCNot0 = 512
        ' <summary>Alias expansion, not a real instruction.</summary>
        IsAlias = 1024
    End Enum

    Public Class AgcOpcode
        Public Property Name As String = ""
        ''' <summary>Opcode field including the quarter code (e.g. 060000 for AD).</summary>
        Public Property Opcode As Integer
        Public Property Flags As OperandFlags = OperandFlags.None
        Public Property AliasOperator As String = ""
        Public Property AliasOperand As String = ""
        Public Property IsInterpretive As Boolean
        Public Property InterpreterCode As Integer
        Public Property InterpreterOperands As Integer
        Public Property InterpreterSwitchKind As Integer
        Public Property InterpreterNnnn As Integer
        Public Property ArgType0 As Integer
        Public Property ArgType1 As Integer
    End Class

    Public Module AgcOpcodeTable

        Private ReadOnly _basic As New Dictionary(Of String, AgcOpcode)(StringComparer.Ordinal)
        Private ReadOnly _interpretive As New Dictionary(Of String, AgcOpcode)(StringComparer.Ordinal)

        ''' <summary>
        ''' True when the mnemonic is a real instruction (not a pseudo-op).  Used
        ''' when deciding whether an Opcode-looking field is actually a label.
        ''' </summary>
        Public Function IsBasicOpcode(name As String) As Boolean
            Return _basic.ContainsKey(name)
        End Function

        Public Function TryGetBasic(name As String, ByRef op As AgcOpcode) As Boolean
            Return _basic.TryGetValue(name, op)
        End Function

        Public Function TryGetInterpretive(name As String, ByRef op As AgcOpcode) As Boolean
            Return _interpretive.TryGetValue(name, op)
        End Function

        Public ReadOnly Property BasicOpcodes As IEnumerable(Of AgcOpcode)
            Get
                Return _basic.Values
            End Get
        End Property

        Public ReadOnly Property InterpretiveOpcodes As IEnumerable(Of AgcOpcode)
            Get
                Return _interpretive.Values
            End Get
        End Property

        ''' <summary>Mnemonic -> basic opcode value, for the disassembler.</summary>
        Public ReadOnly Property BasicByOpcode As New Dictionary(Of Integer, String)

        Sub New()
            BuildBasic()
            BuildInterpretive()
            BuildReverseTables()
        End Sub

        Private Sub AddBasic(name As String, opcode As Integer, Optional flags As OperandFlags = OperandFlags.None)
            Dim o As New AgcOpcode()
            o.Name = name
            o.Opcode = opcode
            o.Flags = flags
            _basic(name) = o
        End Sub

        Private Sub AddAlias(name As String, targetOp As String, targetOperand As String)
            Dim o As New AgcOpcode()
            o.Name = name
            o.Flags = OperandFlags.IsAlias
            o.AliasOperator = targetOp
            o.AliasOperand = targetOperand
            _basic(name) = o
        End Sub

        Private Sub AddInterp(name As String, code As Integer,
                              Optional operands As Integer = 0,
                              Optional switchKind As Integer = 0,
                              Optional nnnn As Integer = 0,
                              Optional arg0 As Integer = 0,
                              Optional arg1 As Integer = 0)
            Dim o As New AgcOpcode()
            o.Name = name
            o.IsInterpretive = True
            o.InterpreterCode = code
            o.InterpreterOperands = operands
            o.InterpreterSwitchKind = switchKind
            o.InterpreterNnnn = nnnn
            o.ArgType0 = arg0
            o.ArgType1 = arg1
            _interpretive(name) = o
        End Sub

        ' ------------------------------------------------------------------
        ' Block II basic instruction set
        ' ------------------------------------------------------------------
        Private Sub BuildBasic()
            ' -- Non-extended (opcode bits 9-11).
            AddBasic("TC", &H0, OperandFlags.Number)
            AddBasic("CCS", &H1000, OperandFlags.QC0 Or OperandFlags.Number)
            AddBasic("TCF", &H2000, OperandFlags.QCNot0)
            AddBasic("DAS", &H4000, OperandFlags.QC0 Or OperandFlags.Plus1)
            AddBasic("LXCH", &H5000, OperandFlags.QC2 Or OperandFlags.Number)
            AddBasic("INCR", &H6000, OperandFlags.QC2)
            AddBasic("ADS", &H7000, OperandFlags.QC2)
            AddBasic("CA", &H9000, OperandFlags.Number)
            AddBasic("CAF", &H9000, OperandFlags.Fixed Or OperandFlags.Number)
            AddBasic("CAE", &H9000, OperandFlags.Erasable Or OperandFlags.Number)
            AddBasic("CS", &HA000, OperandFlags.Number)
            AddBasic("INDEX", &HC000)
            AddBasic("NDX", &HC000)
            AddBasic("DXCH", &HD000, OperandFlags.QC1 Or OperandFlags.Plus1 Or OperandFlags.Number)
            AddBasic("TS", &HE000, OperandFlags.QC2 Or OperandFlags.Number)
            AddBasic("XCH", &HF000, OperandFlags.QC3 Or OperandFlags.Number)
            AddBasic("AD", &H10000)
            AddBasic("MASK", &H18000)
            AddBasic("MSK", &H18000)

            ' -- Extended (opcode bits 9-14, EXTEND required).
            AddBasic("READ", &H20000, OperandFlags.Extended Or OperandFlags.Number)
            AddBasic("WRITE", &H24000, OperandFlags.Extended Or OperandFlags.Number)
            AddBasic("RAND", &H28000, OperandFlags.Extended Or OperandFlags.Number)
            AddBasic("WAND", &H2C000, OperandFlags.Extended Or OperandFlags.Number)
            AddBasic("ROR", &H30000, OperandFlags.Extended Or OperandFlags.Number)
            AddBasic("WOR", &H34000, OperandFlags.Extended Or OperandFlags.Number)
            AddBasic("RXOR", &H38000, OperandFlags.Extended Or OperandFlags.Number)
            AddBasic("EDRUPT", &H3C000, OperandFlags.Extended)
            AddBasic("DV", &H44000, OperandFlags.Extended Or OperandFlags.QCNot0)
            AddBasic("BZF", &H40000, OperandFlags.Extended Or OperandFlags.QCNot0)
            AddBasic("MSU", &H48000, OperandFlags.Extended Or OperandFlags.QCNot0)
            AddBasic("QXCH", &H50000, OperandFlags.Extended Or OperandFlags.QC2 Or OperandFlags.Number)
            AddBasic("AUG", &H54000, OperandFlags.Extended Or OperandFlags.QC2)
            AddBasic("DIM", &H58000, OperandFlags.Extended Or OperandFlags.QC2)
            AddBasic("DCA", &H5C000, OperandFlags.Extended Or OperandFlags.Plus1 Or OperandFlags.Number)
            AddBasic("DCS", &H60000, OperandFlags.Extended Or OperandFlags.Plus1 Or OperandFlags.Number)
            AddBasic("SU", &H64000, OperandFlags.Extended Or OperandFlags.QCNot0)
            AddBasic("BZMF", &H68000, OperandFlags.Extended Or OperandFlags.QCNot0)
            AddBasic("MP", &H70000, OperandFlags.Extended)

            ' -- Aliases (the AGC assembler expands these in place).
            AddAlias("COM", "CS", "A")
            AddAlias("DCOM", "DCS", "A")
            AddAlias("DDOUBL", "DAS", "A")
            AddAlias("DOUBLE", "AD", "A")
            AddAlias("DTCB", "DXCH", "Z")
            AddAlias("DTCF", "DXCH", "$4")
            AddAlias("EXTEND", "TC", "$6")
            AddAlias("INHINT", "TC", "$4")
            AddAlias("RELINT", "TC", "$3")
            AddAlias("RESUME", "INDEX", "$17")
            AddAlias("RETURN", "TC", "Q")
            AddAlias("OVSK", "TS", "A")
            AddAlias("SQUARE", "MP", "A")
            AddAlias("TCAA", "TS", "Z")
            AddAlias("XLQ", "TC", "L")
            AddAlias("XXALQ", "TC", "A")
            AddAlias("ZL", "LXCH", "$7")
            AddAlias("ZQ", "QXCH", "$7")
            AddAlias("NOOP", "TCF", "+0")
        End Sub

        ''' <summary>Extended opcode bit pattern for an instruction, or 0.</summary>
        Public Function ExtendedOpcodeBits(flags As OperandFlags) As Integer
            Return 0
        End Function

        ''' <summary>Build the mnemonic lookup tables used by the disassembler.</summary>
        Private Sub BuildReverseTables()
            For Each kv As KeyValuePair(Of String, AgcOpcode) In _basic
                Dim o As AgcOpcode = kv.Value
                If (o.Flags And OperandFlags.IsAlias) <> 0 Then Continue For
                If Not BasicByOpcode.ContainsKey(o.Opcode) Then BasicByOpcode(o.Opcode) = o.Name
            Next
        End Sub

        ' ------------------------------------------------------------------
        ' Interpretive (Danzig) instruction set
        ' ------------------------------------------------------------------
        Private Sub BuildInterpretive()
            AddInterp("ABS", &H58)
            AddInterp("ABVAL", &H58)
            AddInterp("ARCCOS", &H28)
            AddInterp("ACOS", &H28)
            AddInterp("ARCSIN", &H20)
            AddInterp("ASIN", &H20)
            AddInterp("AXC,1", &HE, 1)
            AddInterp("AXC,2", &HA, 1)
            AddInterp("AXT,1", &H6, 1)
            AddInterp("AXT,2", &H2, 1)
            AddInterp("BDDV", &H49, 1, 0, 0, 1)
            AddInterp("BDDV*", &H4B, 1, 0, 0, 1)
            AddInterp("BDSU", &H6D, 1, 0, 0, 1)
            AddInterp("BDSU*", &H6F, 1, 0, 0, 1)
            AddInterp("BHIZ", &H66, 1)
            AddInterp("BMN", &H5E, 1)
            AddInterp("BOFCLR", &H72, 2, 1, &HA1, 1)
            AddInterp("BOF", &H72, 2, 1, &HE1, 1)
            AddInterp("BOFF", &H72, 2, 1, &HE1, 1)
            AddInterp("BOFINV", &H72, 2, 1, &H61, 1)
            AddInterp("BOFSET", &H72, 2, 1, &H21, 1)
            AddInterp("BON", &H72, 2, 1, &HC1, 1)
            AddInterp("BONCLR", &H72, 2, 1, &H81, 1)
            AddInterp("BONINV", &H72, 2, 1, &H41, 1)
            AddInterp("BONSET", &H72, 2, 1, &H1, 1)
            AddInterp("BOV", &H7E, 1)
            AddInterp("BOVB", &H7A, 1)
            AddInterp("BPL", &H5A, 1)
            AddInterp("BVSU", &H59, 1, 0, 0, 1)
            AddInterp("BVSU*", &H5B, 1, 0, 0, 1)
            AddInterp("BZE", &H52, 1)
            AddInterp("CALL", &H6A, 1)
            AddInterp("CALRB", &H6A, 1)
            AddInterp("CCALL", &H35, 2, 0, 0, 1)
            AddInterp("CCALL*", &H37, 2, 0, 0, 1)
            AddInterp("CGOTO", &H11, 2, 0, 0, 1)
            AddInterp("CGOTO*", &H13, 2, 0, 0, 1)
            AddInterp("CLEAR", &H72, 1, 1, &HB1)
            AddInterp("CLR", &H72, 1, 1, &HB1)
            AddInterp("CLRGO", &H72, 2, 1, &H91)
            AddInterp("COS", &H18)
            AddInterp("COSINE", &H18)
            AddInterp("DAD", &H71, 1, 0, 0, 1)
            AddInterp("DAD*", &H73, 1, 0, 0, 1)
            AddInterp("DCOMP", &H40)
            AddInterp("DDV", &H45, 1, 0, 0, 1)
            AddInterp("DDV*", &H47, 1, 0, 0, 1)
            AddInterp("DLOAD", &H19, 1, 0, 0, 1)
            AddInterp("DLOAD*", &H1B, 1, 0, 0, 1)
            AddInterp("DMP", &H79, 1, 0, 0, 1)
            AddInterp("DMP*", &H7B, 1, 0, 0, 1)
            AddInterp("DMPR", &H41, 1, 0, 0, 1)
            AddInterp("DMPR*", &H43, 1, 0, 0, 1)
            AddInterp("DOT", &H5D, 1, 0, 0, 1)
            AddInterp("DOT*", &H5F, 1, 0, 0, 1)
            AddInterp("DSQ", &H30)
            AddInterp("DSU", &H69, 1, 0, 0, 1)
            AddInterp("DSU*", &H6B, 1, 0, 0, 1)
            AddInterp("EXIT", &H0)
            AddInterp("GOTO", &H56, 1)
            AddInterp("INCR,1", &H36, 1)
            AddInterp("INCR,2", &H32, 1)
            AddInterp("INVERT", &H72, 1, 1, &H71)
            AddInterp("INVGO", &H72, 2, 1, &H51)
            AddInterp("ITA", &H6E, 1)
            AddInterp("ITCQ", &H70)
            AddInterp("LXA,1", &H16, 1)
            AddInterp("LXA,2", &H12, 1)
            AddInterp("LXC,1", &H1E, 1)
            AddInterp("LXC,2", &H1A, 1)
            AddInterp("MXV", &H2D, 1, 0, 0, 1)
            AddInterp("MXV*", &H2F, 1, 0, 0, 1)
            AddInterp("NORM", &H3D, 1, 0, 0, 1)
            AddInterp("NORM*", &H3F, 1, 0, 0, 1)
            AddInterp("PDDL", &H29, 1, 0, 0, 1)
            AddInterp("PDDL*", &H2B, 1, 0, 0, 1)
            AddInterp("PDVL", &H31, 1, 0, 0, 1)
            AddInterp("PDVL*", &H33, 1, 0, 0, 1)
            AddInterp("PUSH", &H78)
            AddInterp("ROUND", &H38)
            AddInterp("RTB", &H62, 1)
            AddInterp("RVQ", &H70)
            AddInterp("SET", &H72, 1, 1, &H31)
            AddInterp("SETGO", &H72, 2, 1, &H11)
            AddInterp("SETPD", &H7D, 1, 0, 0, 1)
            AddInterp("SIGN", &H9, 1, 0, 0, 1)
            AddInterp("SIGN*", &HB, 1, 0, 0, 1)
            AddInterp("SIN", &H10)
            AddInterp("SINE", &H10)
            AddInterp("SL", &H4D, 1, 2, &H8282, 1)
            AddInterp("SL*", &H4F, 1, 2, &H8282, 1)
            AddInterp("SLOAD", &H21, 1, 0, 0, 1)
            AddInterp("SLOAD*", &H23, 1, 0, 0, 1)
            AddInterp("SL1", &H14)
            AddInterp("SL1R", &H4)
            AddInterp("SL2", &H34)
            AddInterp("SL2R", &H24)
            AddInterp("SL3", &H54)
            AddInterp("SL3R", &H44)
            AddInterp("SL4", &H74)
            AddInterp("SL4R", &H64)
            AddInterp("SLR", &H4D, 1, 2, &H8A82, 1)
            AddInterp("SLR*", &H4F, 1, 2, &H8A82, 1)
            AddInterp("SQRT", &H8)
            AddInterp("SR", &H4D, 1, 2, &H8602, 1)
            AddInterp("SR*", &H4F, 1, 2, &H8602, 1)
            AddInterp("SR1", &H1C)
            AddInterp("SR1R", &HC)
            AddInterp("SR2", &H3C)
            AddInterp("SR2R", &H2C)
            AddInterp("SR3", &H5C)
            AddInterp("SR3R", &H4C)
            AddInterp("SR4", &H7C)
            AddInterp("SR4R", &H6C)
            AddInterp("SRR", &H4D, 1, 2, &H8E02, 1)
            AddInterp("SRR*", &H4F, 1, 2, &H8E02, 1)
            AddInterp("SSP", &H25, 2, 0, 0, 1)
            AddInterp("SSP*", &H27, 1, 0, 0, 1)
            AddInterp("STADR", &H68)
            AddInterp("STQ", &H6E, 1)
            AddInterp("SXA,1", &H26, 1)
            AddInterp("SXA,2", &H22, 1)
            AddInterp("TAD", &H5, 1, 0, 0, 1)
            AddInterp("TAD*", &H7, 1, 0, 0, 1)
            AddInterp("TIX,1", &H3E, 1)
            AddInterp("TIX,2", &H3A, 1)
            AddInterp("TLOAD", &H15, 1, 0, 0, 1)
            AddInterp("TLOAD*", &H17, 1, 0, 0, 1)
            AddInterp("UNIT", &H50)
            AddInterp("V/SC", &H1D, 1, 0, 0, 1)
            AddInterp("V/SC*", &H1F, 1, 0, 0, 1)
            AddInterp("VAD", &H51, 1, 0, 0, 1)
            AddInterp("VAD*", &H53, 1, 0, 0, 1)
            AddInterp("VCOMP", &H40)
            AddInterp("VDEF", &H48)
            AddInterp("VLOAD", &H1, 1, 0, 0, 1)
            AddInterp("VLOAD*", &H3, 1, 0, 0, 1)
            AddInterp("VPROJ", &H65, 1, 0, 0, 1)
            AddInterp("VPROJ*", &H67, 1, 0, 0, 1)
            AddInterp("VSL", &H4D, 1, 2, &H8282, 1)
            AddInterp("VSL*", &H4F, 1, 2, &H8282, 1)
            AddInterp("VSL1", &H4)
            AddInterp("VSL2", &H14)
            AddInterp("VSL3", &H24)
            AddInterp("VSL4", &H34)
            AddInterp("VSL5", &H44)
            AddInterp("VSL6", &H54)
            AddInterp("VSL7", &H64)
            AddInterp("VSL8", &H74)
            AddInterp("VSQ", &H60)
            AddInterp("VSR", &H4D, 1, 2, &H8602, 1)
            AddInterp("VSR*", &H4F, 1, 2, &H8602, 1)
            AddInterp("VSR1", &H4)
            AddInterp("VSR2", &H14)
            AddInterp("VSR3", &H24)
            AddInterp("VSR4", &H34)
            AddInterp("VSR5", &H44)
            AddInterp("VSR6", &H54)
            AddInterp("VSR7", &H64)
            AddInterp("VSR8", &H74)
            AddInterp("VSU", &H55, 1, 0, 0, 1)
            AddInterp("VSU*", &H57, 1, 0, 0, 1)
            AddInterp("VXM", &H39, 1, 0, 0, 1)
            AddInterp("VXM*", &H3B, 1, 0, 0, 1)
            AddInterp("VXSC", &HD, 1, 0, 0, 1)
            AddInterp("VXSC*", &HF, 1, 0, 0, 1)
            AddInterp("VXV", &H61, 1, 0, 0, 1)
            AddInterp("VXV*", &H63, 1, 0, 0, 1)
            AddInterp("XAD,1", &H46, 1)
            AddInterp("XAD,2", &H42, 1)
            AddInterp("XCHX,1", &H2E, 1)
            AddInterp("XCHX,2", &H2A, 1)
            AddInterp("XSU,1", &H4E, 1)
            AddInterp("XSU,2", &H4A, 1)
        End Sub

    End Module

End Namespace
