' ============================================================================
'  SymbolTable.vb -- assembler symbol and cross-reference tables.
' ============================================================================

Imports System
Imports System.Collections.Generic

Namespace Agc

    ''' <summary>A single definition of a symbol.</summary>
    Public Class AgcSymbol
        Public Property Name As String
        Public Property Address As AgcAddress
        ''' <summary>True when defined with "=" (logical equivalence, second pass value).</summary>
        Public Property IsEquate As Boolean
        ''' <summary>True when the symbol was defined for the first time in pass 1.</summary>
        Public Property Defined As Boolean
        Public Property File As String
        Public Property Line As Integer
        ''' <summary>Number of times the symbol has been referenced by the source.</summary>
        Public Property ReferenceCount As Integer

        Public Overrides Function ToString() As String
            Return Name & " = " & Address.ToString()
        End Function
    End Class

    ''' <summary>
    ''' Symbol table for the AGC assembler.  Symbols are case sensitive, as they
    ''' are in the original AGC assembler and in the transcribed sources.
    ''' </summary>
    Public Class AgcSymbolTable

        Private ReadOnly _symbols As New Dictionary(Of String, AgcSymbol)(StringComparer.Ordinal)

        Public ReadOnly Property Count As Integer
            Get
                Return _symbols.Count
            End Get
        End Property

        Public ReadOnly Property Symbols As IEnumerable(Of AgcSymbol)
            Get
                Return _symbols.Values
            End Get
        End Property

        Public Function TryGetValue(name As String, ByRef symbol As AgcSymbol) As Boolean
            Return _symbols.TryGetValue(name, symbol)
        End Function

        Public Function Contains(name As String) As Boolean
            Return _symbols.ContainsKey(name)
        End Function

        ''' <summary>
        ''' Define (or redefine) a symbol.  Returns True when the symbol already
        ''' existed, in which case the caller may wish to compare addresses.
        ''' </summary>
        Public Function Define(name As String, address As AgcAddress, isEquate As Boolean,
                               file As String, line As Integer) As Boolean
            Dim existing As AgcSymbol = Nothing
            If _symbols.TryGetValue(name, existing) Then
                ' Record the newest definition so the second pass sees the final value.
                existing.Address = address
                existing.IsEquate = isEquate
                If Not existing.Defined Then
                    existing.Defined = True
                    existing.File = file
                    existing.Line = line
                End If
                Return True
            End If

            Dim s As New AgcSymbol()
            s.Name = name
            s.Address = address
            s.IsEquate = isEquate
            s.Defined = True
            s.File = file
            s.Line = line
            _symbols(name) = s
            Return False
        End Function

        ''' <summary>Note a reference to a symbol (for undefined-symbol reporting).</summary>
        Public Sub NoteReference(name As String)
            Dim s As AgcSymbol = Nothing
            If _symbols.TryGetValue(name, s) Then
                s.ReferenceCount += 1
                Return
            End If
            ' Remember that this name was wanted so it can be reported later.
            Dim missing As AgcSymbol = Nothing
            If Not _missing.TryGetValue(name, missing) Then
                missing = New AgcSymbol()
                missing.Name = name
                missing.ReferenceCount = 1
                _missing(name) = missing
            Else
                missing.ReferenceCount += 1
            End If
        End Sub

        Private ReadOnly _missing As New Dictionary(Of String, AgcSymbol)(StringComparer.Ordinal)

        ''' <summary>Names that were referenced but never defined, sorted by name.</summary>
        Public Function ReferencedButUndefined() As List(Of AgcSymbol)
            Dim list As New List(Of AgcSymbol)()
            For Each s As AgcSymbol In _missing.Values
                If Not _symbols.ContainsKey(s.Name) Then list.Add(s)
            Next
            list.Sort(Function(a, b) String.CompareOrdinal(a.Name, b.Name))
            Return list
        End Function

        Public Sub Clear()
            _symbols.Clear()
            _missing.Clear()
        End Sub

        ''' <summary>All symbols sorted by name, for listings and map files.</summary>
        Public Function SortedSymbols() As List(Of AgcSymbol)
            Dim list As New List(Of AgcSymbol)(_symbols.Values)
            list.Sort(Function(a, b) String.CompareOrdinal(a.Name, b.Name))
            Return list
        End Function

    End Class

End Namespace
