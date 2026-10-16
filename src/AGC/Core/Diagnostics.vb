' ============================================================================
'  Diagnostics.vb -- assembler/runtime message collection.
' ============================================================================

Imports System
Imports System.Collections.Generic
Imports System.Text

Namespace Agc

    Public Enum AgcSeverity
        Info = 0
        Warning = 1
        [Error] = 2
        Fatal = 3
    End Enum

    Public Structure AgcMessage
        Public Severity As AgcSeverity
        Public File As String
        Public Line As Integer
        Public Text As String

        Public Overrides Function ToString() As String
            Dim sb As New StringBuilder()
            If Not String.IsNullOrEmpty(File) Then
                sb.Append(IO.Path.GetFileName(File))
                If Line > 0 Then sb.Append(":").Append(Line)
                sb.Append(": ")
            End If
            sb.Append(Severity.ToString().ToLowerInvariant()).Append(": ").Append(Text)
            Return sb.ToString()
        End Function
    End Structure

    ''' <summary>Collects assembler diagnostics and keeps severity counts.</summary>
    Public Class AgcDiagnostics

        Private ReadOnly _messages As New List(Of AgcMessage)()
        Private ReadOnly _seen As New HashSet(Of String)()

        Public ReadOnly Property Messages As IList(Of AgcMessage)
            Get
                Return _messages
            End Get
        End Property

        Public Property ErrorCount As Integer
        Public Property WarningCount As Integer

        ''' <summary>Maximum number of identical messages to retain.</summary>
        Public Property DuplicateLimit As Integer = 4

        Public ReadOnly Property HasErrors As Boolean
            Get
                Return ErrorCount > 0
            End Get
        End Property

        ''' <summary>
        ''' When set, all diagnostics are discarded.  Used during the assembler's
        ''' first pass, where forward references are expected to be unresolved.
        ''' </summary>
        Public Property Suppress As Boolean

        Public Sub Report(severity As AgcSeverity, file As String, line As Integer, text As String)
            If Suppress Then Return
            Dim msg As New AgcMessage()
            msg.Severity = severity
            msg.File = file
            msg.Line = line
            msg.Text = text

            If severity >= AgcSeverity.[Error] Then ErrorCount += 1
            If severity = AgcSeverity.Warning Then WarningCount += 1

            Dim key As String = severity.ToString() & "|" & text
            If Not _seen.Add(key) Then
                Dim count As Integer = 0
                For Each m As AgcMessage In _messages
                    If m.Severity = severity AndAlso m.Text = text Then count += 1
                Next
                If count >= DuplicateLimit Then Return
            End If
            _messages.Add(msg)
        End Sub

        Public Sub Info(file As String, line As Integer, text As String)
            Report(AgcSeverity.Info, file, line, text)
        End Sub

        Public Sub Warn(file As String, line As Integer, text As String)
            Report(AgcSeverity.Warning, file, line, text)
        End Sub

        Public Sub [Error](file As String, line As Integer, text As String)
            Report(AgcSeverity.[Error], file, line, text)
        End Sub

        Public Sub Fatal(file As String, line As Integer, text As String)
            Report(AgcSeverity.Fatal, file, line, text)
        End Sub

        Public Function FormatAll(Optional maxLines As Integer = 400) As String
            Dim sb As New StringBuilder()
            Dim n As Integer = 0
            For Each m As AgcMessage In _messages
                If n >= maxLines Then
                    sb.AppendLine("... (" & (_messages.Count - n).ToString() & " more messages suppressed)")
                    Exit For
                End If
                sb.AppendLine(m.ToString())
                n += 1
            Next
            Return sb.ToString()
        End Function

    End Class

End Namespace
