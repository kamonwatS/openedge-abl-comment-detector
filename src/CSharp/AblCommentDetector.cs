using System;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using System.Linq;

namespace AblCommentDetector
{
    /// <summary>
    /// OpenEdge ABL Comment Detection Engine - Enhanced Version
    /// Based on grammar patterns from https://github.com/ezequielgandolfi/openedge-zext.git
    /// Now with proper lexical analysis for complex patterns and procedure call analysis
    /// </summary>
    public class AblCommentDetector
    {
        /// <summary>
        /// Result of analyzing a single line
        /// </summary>
        public class LineAnalysisResult
        {
            public int LineNumber { get; set; }
            public string Content { get; set; }
            public LineType Type { get; set; }
            public bool HasExecutableCode { get; set; }
            public bool HasComment { get; set; }
            public int CommentDepthBefore { get; set; }
            public int CommentDepthAfter { get; set; }
            public string Reason { get; set; }
            public bool IsUncalledProcedure { get; set; }
        }

        public enum LineType
        {
            ExecutableCode,
            PureComment,
            MixedContent,
            EmptyLine
        }

        private int _currentCommentDepth = 0;
        private readonly List<LineAnalysisResult> _results = new List<LineAnalysisResult>();
        private readonly Dictionary<string, ProcedureInfo> _procedures = new Dictionary<string, ProcedureInfo>();
        private readonly HashSet<string> _calledProcedures = new HashSet<string>();
        private readonly List<ProcedureBoundary> _procedureBoundaries = new List<ProcedureBoundary>();

        public class ProcedureInfo
        {
            public string Name { get; set; }
            public int LineNumber { get; set; }
            public bool IsCalled { get; set; }
            public string Definition { get; set; }
        }

        public class ProcedureBoundary
        {
            public string Name { get; set; }
            public int StartLine { get; set; }
            public int EndLine { get; set; }
            public bool IsUncalled { get; set; }
        }

        /// <summary>
        /// Enhanced comment state tracker that handles strings and line continuations
        /// </summary>
        private class CommentStateTracker
        {
            private int _depth = 0;
            private bool _inSingleQuoteString = false;
            private bool _inDoubleQuoteString = false;
            private bool _lineContinuation = false;

            public int Depth => _depth;
            public bool InComment => _depth > 0;

            public void ProcessLine(string line)
            {
                // Handle line continuation from previous line
                if (_lineContinuation)
                {
                    _lineContinuation = false;
                    // Continue processing this line in the same state
                }

                var chars = line.ToCharArray();
                for (int i = 0; i < chars.Length; i++)
                {
                    var currentChar = chars[i];
                    var nextChar = i + 1 < chars.Length ? chars[i + 1] : '\0';

                    // Handle escape sequences in strings
                    if ((_inSingleQuoteString || _inDoubleQuoteString) && currentChar == '~')
                    {
                        i++; // Skip next character (it's escaped)
                        continue;
                    }

                    // Handle string boundaries
                    if (!InComment)
                    {
                        if (currentChar == '"' && !_inSingleQuoteString)
                        {
                            _inDoubleQuoteString = !_inDoubleQuoteString;
                            continue;
                        }
                        if (currentChar == '\'' && !_inDoubleQuoteString)
                        {
                            _inSingleQuoteString = !_inSingleQuoteString;
                            continue;
                        }
                    }

                    // Skip comment processing if we're inside a string literal
                    if (_inSingleQuoteString || _inDoubleQuoteString)
                        continue;

                    // Handle comment delimiters
                    if (currentChar == '/' && nextChar == '*')
                    {
                        _depth++;
                        i++; // Skip the '*'
                        continue;
                    }
                    if (currentChar == '*' && nextChar == '/')
                    {
                        if (_depth > 0)
                            _depth--;
                        i++; // Skip the '/'
                        continue;
                    }

                    // Handle single-line comments (only if not in block comment)
                    if (!InComment && currentChar == '/' && nextChar == '/')
                    {
                        // Rest of line is comment, but doesn't affect depth
                        break;
                    }
                }

                // Check for line continuation
                var trimmedLine = line.TrimEnd();
                if (trimmedLine.EndsWith("~"))
                {
                    _lineContinuation = true;
                }

                // Reset string state if no line continuation (strings don't span lines in ABL unless continued)
                if (!_lineContinuation)
                {
                    _inSingleQuoteString = false;
                    _inDoubleQuoteString = false;
                }
            }

            public void Reset()
            {
                _depth = 0;
                _inSingleQuoteString = false;
                _inDoubleQuoteString = false;
                _lineContinuation = false;
            }
        }

        private readonly CommentStateTracker _stateTracker = new CommentStateTracker();

        // Regex patterns for procedure analysis
        private static readonly Regex ProcedureDefinitionRegex = new Regex(
            @"^\s*PROCEDURE\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*[:\.]",
            RegexOptions.IgnoreCase | RegexOptions.Compiled);

        private static readonly Regex FunctionDefinitionRegex = new Regex(
            @"^\s*FUNCTION\s+([a-zA-Z_][a-zA-Z0-9_]*)\s+RETURNS\s+",
            RegexOptions.IgnoreCase | RegexOptions.Compiled);

        private static readonly Regex ProcedureCallRegex = new Regex(
            @"RUN\s+([a-zA-Z_][a-zA-Z0-9_]*)",
            RegexOptions.IgnoreCase | RegexOptions.Compiled);

        private static readonly Regex FunctionCallRegex = new Regex(
            @"([a-zA-Z_][a-zA-Z0-9_]*)\s*\(",
            RegexOptions.IgnoreCase | RegexOptions.Compiled);

        /// <summary>
        /// Analyzes an entire file and returns results for each line
        /// </summary>
        public List<LineAnalysisResult> AnalyzeFile(string filePath)
        {
            if (!File.Exists(filePath))
                throw new FileNotFoundException($"File not found: {filePath}");

            _results.Clear();
            _procedures.Clear();
            _calledProcedures.Clear();
            _procedureBoundaries.Clear();
            _stateTracker.Reset();

            var lines = File.ReadAllLines(filePath);
            
            // First pass: Identify all procedures and calls
            AnalyzeProcedures(lines);
            
            // Second pass: Analyze each line
            for (int i = 0; i < lines.Length; i++)
            {
                var result = AnalyzeLine(lines[i], i + 1);
                _results.Add(result);
            }

            // Third pass: Detect procedure boundaries
            DetectProcedureBoundaries(lines);

            // Fourth pass: Override classifications for uncalled procedures
            OverrideUncalledProcedureLines();

            return _results;
        }

        /// <summary>
        /// First pass: Analyze procedures and their calls
        /// </summary>
        private void AnalyzeProcedures(string[] lines)
        {
            var tempStateTracker = new CommentStateTracker();

            // FIRST PASS: Find all procedure and function definitions
            for (int i = 0; i < lines.Length; i++)
            {
                var line = lines[i];
                var lineNumber = i + 1;
                
                // Track comment state but use more robust detection
                bool startedInComment = tempStateTracker.InComment;
                tempStateTracker.ProcessLine(line);
                bool endedInComment = tempStateTracker.InComment;
                
                // Only skip if the entire line is within a comment block
                if (startedInComment && endedInComment)
                {
                    continue;
                }

                // Skip single-line comments and hash comments (but not mixed content)
                var trimmed = line.TrimStart();
                if ((trimmed.StartsWith("//") || trimmed.StartsWith("#")) && !HasCodeBeforeComment(line, trimmed.StartsWith("//") ? "//" : "#"))
                {
                    continue;
                }

                // Skip complete single-line block comments
                if (IsCompleteSingleLineComment(line))
                {
                    continue;
                }

                // Look for procedure definitions
                var procMatch = ProcedureDefinitionRegex.Match(line);
                if (procMatch.Success)
                {
                    var procName = procMatch.Groups[1].Value;
                    if (!_procedures.ContainsKey(procName.ToUpper()))
                    {
                        _procedures[procName.ToUpper()] = new ProcedureInfo
                        {
                            Name = procName,
                            LineNumber = lineNumber,
                            IsCalled = false,
                            Definition = line.Trim()
                        };
                    }
                }

                // Look for function definitions
                var funcMatch = FunctionDefinitionRegex.Match(line);
                if (funcMatch.Success)
                {
                    var funcName = funcMatch.Groups[1].Value;
                    if (!_procedures.ContainsKey(funcName.ToUpper()))
                    {
                        _procedures[funcName.ToUpper()] = new ProcedureInfo
                        {
                            Name = funcName,
                            LineNumber = lineNumber,
                            IsCalled = false,
                            Definition = line.Trim()
                        };
                    }
                }
            }

            // Reset state tracker for second pass
            tempStateTracker.Reset();
            
            // SECOND PASS: Find all procedure and function calls
            for (int i = 0; i < lines.Length; i++)
            {
                var line = lines[i];
                var lineNumber = i + 1;
                
                // Track comment state but use more robust detection
                bool startedInComment = tempStateTracker.InComment;
                tempStateTracker.ProcessLine(line);
                bool endedInComment = tempStateTracker.InComment;
                
                // Only skip if the entire line is within a comment block
                if (startedInComment && endedInComment)
                {
                    continue;
                }

                // Skip single-line comments and hash comments (but not mixed content)
                var trimmed = line.TrimStart();
                if ((trimmed.StartsWith("//") || trimmed.StartsWith("#")) && !HasCodeBeforeComment(line, trimmed.StartsWith("//") ? "//" : "#"))
                {
                    continue;
                }

                // Skip complete single-line block comments
                if (IsCompleteSingleLineComment(line))
                {
                    continue;
                }

                // Look for procedure calls (RUN statements) - enhanced detection
                var runMatches = ProcedureCallRegex.Matches(line);
                foreach (Match match in runMatches)
                {
                    var procName = match.Groups[1].Value.ToUpper();
                    
                    _calledProcedures.Add(procName);
                    if (_procedures.ContainsKey(procName))
                    {
                        _procedures[procName].IsCalled = true;
                    }
                }

                // Look for function calls (function_name())
                var funcMatches = FunctionCallRegex.Matches(line);
                foreach (Match match in funcMatches)
                {
                    var funcName = match.Groups[1].Value.ToUpper();
                    
                    // Skip built-in ABL functions and keywords
                    if (!IsBuiltInFunction(funcName))
                    {
                        _calledProcedures.Add(funcName);
                        if (_procedures.ContainsKey(funcName))
                        {
                            _procedures[funcName].IsCalled = true;
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Helper method to check if there's code before a comment marker
        /// </summary>
        private bool HasCodeBeforeComment(string line, string commentMarker)
        {
            var index = line.IndexOf(commentMarker);
            if (index <= 0) return false;
            var beforeComment = line.Substring(0, index).Trim();
            return !string.IsNullOrEmpty(beforeComment);
        }

        /// <summary>
        /// Check if a function name is a built-in ABL function
        /// </summary>
        private bool IsBuiltInFunction(string funcName)
        {
            var builtInFunctions = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
            {
                "STRING", "INTEGER", "DECIMAL", "DATE", "TODAY", "NOW", "TIME",
                "SUBSTRING", "LENGTH", "TRIM", "UPPER", "LOWER", "REPLACE",
                "IF", "THEN", "ELSE", "AND", "OR", "NOT", "YEAR", "MONTH", "DAY",
                "CAN-DO", "CAN-FIND", "AVAILABLE", "LOCKED", "CURRENT-LANGUAGE",
                "USERID", "PROGRAM-NAME", "FILL", "CHR", "ASC", "INDEX", "R-INDEX",
                "ABSOLUTE", "TRUNCATE", "ROUND", "SQRT", "EXP", "LOG", "RANDOM",
                "MINIMUM", "MAXIMUM", "ENTRY", "NUM-ENTRIES", "LOOKUP"
            };
            
            return builtInFunctions.Contains(funcName);
        }

        /// <summary>
        /// Analyzes a single line with enhanced lexical analysis
        /// </summary>
        public LineAnalysisResult AnalyzeLine(string line, int lineNumber)
        {
            var result = new LineAnalysisResult
            {
                LineNumber = lineNumber,
                Content = line,
                CommentDepthBefore = _stateTracker.Depth
            };

            // Handle empty or whitespace-only lines
            if (string.IsNullOrWhiteSpace(line))
            {
                result.Type = LineType.EmptyLine;
                result.HasExecutableCode = false;
                result.HasComment = false;
                result.Reason = "Empty or whitespace-only line";
                result.CommentDepthAfter = _stateTracker.Depth;
                return result;
            }

            // Check if we're starting inside a comment block
            bool startedInComment = _stateTracker.InComment;

            // Process the line to update state
            _stateTracker.ProcessLine(line);

            // Determine line type based on content analysis
            result = AnalyzeLineContent(line, result, startedInComment);
            result.CommentDepthAfter = _stateTracker.Depth;

            // Check if this line defines an uncalled procedure
            CheckForUncalledProcedure(line, result);

            return result;
        }

        /// <summary>
        /// Check if the line defines an uncalled procedure
        /// </summary>
        private void CheckForUncalledProcedure(string line, LineAnalysisResult result)
        {
            if (result.Type != LineType.ExecutableCode && result.Type != LineType.MixedContent)
                return;

            var procMatch = ProcedureDefinitionRegex.Match(line);
            if (procMatch.Success)
            {
                var procName = procMatch.Groups[1].Value.ToUpper();
                if (_procedures.ContainsKey(procName) && !_procedures[procName].IsCalled)
                {
                    result.IsUncalledProcedure = true;
                }
            }

            var funcMatch = FunctionDefinitionRegex.Match(line);
            if (funcMatch.Success)
            {
                var funcName = funcMatch.Groups[1].Value.ToUpper();
                if (_procedures.ContainsKey(funcName) && !_procedures[funcName].IsCalled)
                {
                    result.IsUncalledProcedure = true;
                }
            }
        }

        private LineAnalysisResult AnalyzeLineContent(string line, LineAnalysisResult result, bool startedInComment)
        {
            var trimmedLine = line.TrimStart();

            // If the entire line is within a comment block
            if (startedInComment && _stateTracker.InComment)
            {
                result.Type = LineType.PureComment;
                result.HasExecutableCode = false;
                result.HasComment = true;
                result.Reason = "Line is entirely within multi-line comment block";
                return result;
            }

            // If line started in comment but ends outside (has code after comment)
            if (startedInComment && !_stateTracker.InComment)
            {
                if (HasCodeAfterCommentEnd(line))
                {
                    result.Type = LineType.MixedContent;
                    result.HasExecutableCode = true;
                    result.HasComment = true;
                    result.Reason = "Line ends comment block and contains executable code";
                    return result;
                }
                else
                {
                    result.Type = LineType.PureComment;
                    result.HasExecutableCode = false;
                    result.HasComment = true;
                    result.Reason = "Line ends comment block with no executable code";
                    return result;
                }
            }

            // Check for pure single-line comment
            if (trimmedLine.StartsWith("//"))
            {
                result.Type = LineType.PureComment;
                result.HasExecutableCode = false;
                result.HasComment = true;
                result.Reason = "Single-line comment starting with //";
                return result;
            }

            // Check for hash comment (preprocessor directive)
            if (trimmedLine.StartsWith("#"))
            {
                result.Type = LineType.PureComment;
                result.HasExecutableCode = false;
                result.HasComment = true;
                result.Reason = "Hash comment/preprocessor directive starting with #";
                return result;
            }

            // Check for complete single-line block comment
            if (IsCompleteSingleLineComment(line))
            {
                result.Type = LineType.PureComment;
                result.HasExecutableCode = false;
                result.HasComment = true;
                result.Reason = "Complete single-line block comment /* ... */";
                return result;
            }

            // Check if line starts a comment but also has code
            if (!startedInComment && _stateTracker.InComment)
            {
                if (HasCodeBeforeCommentStart(line))
                {
                    result.Type = LineType.MixedContent;
                    result.HasExecutableCode = true;
                    result.HasComment = true;
                    result.Reason = "Line has executable code before comment start";
                    return result;
                }
                else
                {
                    result.Type = LineType.PureComment;
                    result.HasExecutableCode = false;
                    result.HasComment = true;
                    result.Reason = "Line starts multi-line comment block";
                    return result;
                }
            }

            // Check for inline comments (code with trailing comment)
            if (HasInlineComment(line))
            {
                result.Type = LineType.MixedContent;
                result.HasExecutableCode = true;
                result.HasComment = true;
                result.Reason = "Line contains both executable code and inline comment (treated as executable per requirement)";
                return result;
            }

            // Default to executable code
            result.Type = LineType.ExecutableCode;
            result.HasExecutableCode = true;
            result.HasComment = false;
            result.Reason = "Line contains executable code only";
            return result;
        }

        private bool HasCodeAfterCommentEnd(string line)
        {
            var inString = false;
            var stringChar = '\0';
            
            for (int i = 0; i < line.Length - 1; i++)
            {
                var current = line[i];
                var next = line[i + 1];

                // Handle string boundaries
                if (!inString && (current == '"' || current == '\''))
                {
                    inString = true;
                    stringChar = current;
                    continue;
                }
                if (inString && current == stringChar && (i == 0 || line[i - 1] != '~'))
                {
                    inString = false;
                    continue;
                }
                if (inString) continue;

                // Look for comment end
                if (current == '*' && next == '/')
                {
                    var afterComment = line.Substring(i + 2).Trim();
                    return !string.IsNullOrEmpty(afterComment) && !afterComment.StartsWith("//");
                }
            }
            return false;
        }

        private bool HasCodeBeforeCommentStart(string line)
        {
            var inString = false;
            var stringChar = '\0';
            
            for (int i = 0; i < line.Length - 1; i++)
            {
                var current = line[i];
                var next = line[i + 1];

                // Handle string boundaries
                if (!inString && (current == '"' || current == '\''))
                {
                    inString = true;
                    stringChar = current;
                    continue;
                }
                if (inString && current == stringChar && (i == 0 || line[i - 1] != '~'))
                {
                    inString = false;
                    continue;
                }
                if (inString) continue;

                // Look for comment start
                if (current == '/' && next == '*')
                {
                    var beforeComment = line.Substring(0, i).Trim();
                    return !string.IsNullOrEmpty(beforeComment);
                }
            }
            return false;
        }

        private bool IsCompleteSingleLineComment(string line)
        {
            var trimmed = line.Trim();
            if (!trimmed.StartsWith("/*") || !trimmed.EndsWith("*/"))
                return false;

            // Use state tracker to verify it's actually complete
            var tempTracker = new CommentStateTracker();
            tempTracker.ProcessLine(line);
            return tempTracker.Depth == 0; // Should return to depth 0 if complete
        }

        private bool HasInlineComment(string line)
        {
            var inString = false;
            var stringChar = '\0';
            
            for (int i = 0; i < line.Length - 1; i++)
            {
                var current = line[i];
                var next = line[i + 1];

                // Handle string boundaries
                if (!inString && (current == '"' || current == '\''))
                {
                    inString = true;
                    stringChar = current;
                    continue;
                }
                if (inString && current == stringChar && (i == 0 || line[i - 1] != '~'))
                {
                    inString = false;
                    continue;
                }
                if (inString) continue;

                // Look for comment markers outside strings
                if (current == '/' && next == '/')
                {
                    var beforeComment = line.Substring(0, i).Trim();
                    return !string.IsNullOrEmpty(beforeComment);
                }
                if (current == '/' && next == '*')
                {
                    var beforeComment = line.Substring(0, i).Trim();
                    if (!string.IsNullOrEmpty(beforeComment))
                        return true;
                }
            }
            return false;
        }

        /// <summary>
        /// Detect the start and end lines of each procedure
        /// </summary>
        private void DetectProcedureBoundaries(string[] lines)
        {
            var tempStateTracker = new CommentStateTracker();
            var procedureStack = new Stack<ProcedureBoundary>();

            for (int i = 0; i < lines.Length; i++)
            {
                var line = lines[i];
                var lineNumber = i + 1;
                
                // Track comment state
                bool startedInComment = tempStateTracker.InComment;
                tempStateTracker.ProcessLine(line);
                bool endedInComment = tempStateTracker.InComment;
                
                // Skip lines in comments
                if (startedInComment && endedInComment)
                    continue;

                var trimmed = line.TrimStart();
                if ((trimmed.StartsWith("//") || trimmed.StartsWith("#")) && !HasCodeBeforeComment(line, trimmed.StartsWith("//") ? "//" : "#"))
                    continue;

                if (IsCompleteSingleLineComment(line))
                    continue;

                // Look for procedure start
                var procMatch = ProcedureDefinitionRegex.Match(line);
                if (procMatch.Success)
                {
                    var procName = procMatch.Groups[1].Value;
                    var isUncalled = _procedures.ContainsKey(procName.ToUpper()) && !_procedures[procName.ToUpper()].IsCalled;
                    
                    var boundary = new ProcedureBoundary
                    {
                        Name = procName,
                        StartLine = lineNumber,
                        EndLine = lineNumber, // Will be updated when END PROCEDURE is found
                        IsUncalled = isUncalled
                    };
                    procedureStack.Push(boundary);
                }

                // Look for function start
                var funcMatch = FunctionDefinitionRegex.Match(line);
                if (funcMatch.Success)
                {
                    var funcName = funcMatch.Groups[1].Value;
                    var isUncalled = _procedures.ContainsKey(funcName.ToUpper()) && !_procedures[funcName.ToUpper()].IsCalled;
                    
                    var boundary = new ProcedureBoundary
                    {
                        Name = funcName,
                        StartLine = lineNumber,
                        EndLine = lineNumber, // Will be updated when END FUNCTION is found
                        IsUncalled = isUncalled
                    };
                    procedureStack.Push(boundary);
                }

                // Look for procedure/function end
                if (trimmed.ToUpper().Contains("END PROCEDURE") || trimmed.ToUpper().Contains("END FUNCTION"))
                {
                    if (procedureStack.Count > 0)
                    {
                        var boundary = procedureStack.Pop();
                        boundary.EndLine = lineNumber;
                        _procedureBoundaries.Add(boundary);
                    }
                }
            }

            // Handle any unclosed procedures (end at end of file)
            while (procedureStack.Count > 0)
            {
                var boundary = procedureStack.Pop();
                boundary.EndLine = lines.Length;
                _procedureBoundaries.Add(boundary);
            }
        }

        /// <summary>
        /// Override line classifications for lines within uncalled procedures
        /// </summary>
        private void OverrideUncalledProcedureLines()
        {
            foreach (var boundary in _procedureBoundaries)
            {
                if (boundary.IsUncalled)
                {
                    // Mark all lines within this uncalled procedure as uncalled
                    for (int i = boundary.StartLine - 1; i < boundary.EndLine && i < _results.Count; i++)
                    {
                        _results[i].IsUncalledProcedure = true;
                        _results[i].Type = LineType.ExecutableCode; // Treat as executable for output formatting
                        _results[i].Reason = $"Line within uncalled procedure '{boundary.Name}'";
                    }
                }
            }
        }

        /// <summary>
        /// Generates a report showing analysis results
        /// </summary>
        public void GenerateReport(string inputFile, string outputFile)
        {
            var results = AnalyzeFile(inputFile);
            
            // Change output file extension to .w if not already specified
            if (!outputFile.EndsWith(".w"))
            {
                outputFile = Path.ChangeExtension(outputFile, ".w");
            }
            
            using var writer = new StreamWriter(outputFile, false, Encoding.UTF8);
            
            foreach (var result in results)
            {
                if (result.Type == LineType.PureComment)
                {
                    writer.WriteLine($"/*[COMMENT]*/          {result.Content}");
                }
                else if (result.Type == LineType.EmptyLine)
                {
                    writer.WriteLine($"/*[BLANK]*/            {result.Content}");
                }
                else if (result.IsUncalledProcedure)
                {
                    writer.WriteLine($"/*[UNCALLED]*/         {result.Content}");
                }
                else if (result.Type == LineType.ExecutableCode || result.Type == LineType.MixedContent)
                {
                    writer.WriteLine($"/*[EXECUTABLE]*/       {result.Content}");
                }
                else
                {
                    writer.WriteLine($"{result.Content}");
                }
            }
        }

        /// <summary>
        /// Public method to calculate statistics from analysis results
        /// </summary>
        public (int ExecutableLines, double ExecutablePercentage,
                int CommentLines, double CommentPercentage,
                int MixedLines, double MixedPercentage,
                int EmptyLines, double EmptyPercentage,
                int UncalledProcedures, double UncalledPercentage) CalculateStatistics(List<LineAnalysisResult> results)
        {
            var total = results.Count;
            var executable = results.Count(r => r.Type == LineType.ExecutableCode && !r.IsUncalledProcedure);
            var comments = results.Count(r => r.Type == LineType.PureComment);
            var mixed = results.Count(r => r.Type == LineType.MixedContent && !r.IsUncalledProcedure);
            var empty = results.Count(r => r.Type == LineType.EmptyLine);
            var uncalled = results.Count(r => r.IsUncalledProcedure);

            return (
                ExecutableLines: executable,
                ExecutablePercentage: (double)executable / total * 100,
                CommentLines: comments,
                CommentPercentage: (double)comments / total * 100,
                MixedLines: mixed,
                MixedPercentage: (double)mixed / total * 100,
                EmptyLines: empty,
                EmptyPercentage: (double)empty / total * 100,
                UncalledProcedures: uncalled,
                UncalledPercentage: (double)uncalled / total * 100
            );
        }

        /// <summary>
        /// Get information about detected procedures
        /// </summary>
        public Dictionary<string, ProcedureInfo> GetProcedureInfo()
        {
            return _procedures;
        }

        /// <summary>
        /// Resets the detector state for analyzing a new file
        /// </summary>
        public void Reset()
        {
            _stateTracker.Reset();
            _results.Clear();
            _procedures.Clear();
            _calledProcedures.Clear();
            _procedureBoundaries.Clear();
        }
    }
}
 