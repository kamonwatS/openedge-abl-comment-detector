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
            public string Content { get; set; } = string.Empty;
            public LineType Type { get; set; }
            public bool HasExecutableCode { get; set; }
            public bool HasComment { get; set; }
            public int CommentDepthBefore { get; set; }
            public int CommentDepthAfter { get; set; }
            public string Reason { get; set; } = string.Empty;
            public bool IsUncalledProcedure { get; set; }
        }

        /// <summary>
        /// Defines the different types of lines in OpenEdge ABL code
        /// </summary>
        public enum LineType
        {
            /// <summary>Line contains only executable code</summary>
            ExecutableCode,
            
            /// <summary>Line contains only comments</summary>
            PureComment,
            
            /// <summary>Line contains both executable code and comments</summary>
            MixedContent,
            
            /// <summary>Line is empty or contains only whitespace</summary>
            EmptyLine
        }

        // Storage for line-by-line analysis results
        private readonly List<LineAnalysisResult> _results = new List<LineAnalysisResult>();
        
        // Dictionary mapping procedure names to their detailed information
        private readonly Dictionary<string, ProcedureInfo> _procedures = new Dictionary<string, ProcedureInfo>();
        
        // Set of procedure names that have been called at least once
        private readonly HashSet<string> _calledProcedures = new HashSet<string>();
        
        // List of procedure boundaries (start/end lines) for uncalled detection
        private readonly List<ProcedureBoundary> _procedureBoundaries = new List<ProcedureBoundary>();

        /// <summary>
        /// Information about a procedure or function in the ABL code
        /// </summary>
        public class ProcedureInfo
        {
            /// <summary>Name of the procedure</summary>
            public string Name { get; set; } = string.Empty;
            
            /// <summary>Line number where the procedure is defined</summary>
            public int LineNumber { get; set; }
            
            /// <summary>Whether the procedure is called at least once</summary>
            public bool IsCalled { get; set; }
            
            /// <summary>The text of the procedure definition</summary>
            public string Definition { get; set; } = string.Empty;
        }

        /// <summary>
        /// Records the boundaries (start and end lines) of a procedure
        /// </summary>
        public class ProcedureBoundary
        {
            /// <summary>Name of the procedure</summary>
            public string Name { get; set; } = string.Empty;
            
            /// <summary>Line number where the procedure starts</summary>
            public int StartLine { get; set; }
            
            /// <summary>Line number where the procedure ends</summary>
            public int EndLine { get; set; }
            
            /// <summary>Whether this procedure is never called</summary>
            public bool IsUncalled { get; set; }
        }

        /// <summary>
        /// Enhanced comment state tracker that handles strings and line continuations
        /// This class is responsible for tracking the lexical state while processing code:
        /// - Block comment depth (for nested /* */ comments)
        /// - String literal state (single and double quotes)
        /// - Line continuation using the tilde (~) character
        /// </summary>
        private class CommentStateTracker
        {
            private int _depth = 0;                // Current nesting depth of /* */ comments
            private bool _inSingleQuoteString = false;  // Whether we're in a single-quoted string
            private bool _inDoubleQuoteString = false;  // Whether we're in a double-quoted string
            private bool _lineContinuation = false;     // Whether the previous line ended with ~

            /// <summary>Current comment nesting depth</summary>
            public int Depth => _depth;
            
            /// <summary>Whether currently inside a comment block</summary>
            public bool InComment => _depth > 0;

            /// <summary>
            /// Process a line of code, updating internal state trackers
            /// </summary>
            /// <param name="line">The line of code to process</param>
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
                    // In ABL, the tilde (~) is used to escape characters in strings
                    if ((_inSingleQuoteString || _inDoubleQuoteString) && currentChar == '~')
                    {
                        i++; // Skip next character (it's escaped)
                        continue;
                    }

                    // Handle string boundaries - only if not in a comment
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
                        _depth++; // Increase nesting depth when entering a comment block
                        i++; // Skip the '*'
                        continue;
                    }
                    if (currentChar == '*' && nextChar == '/')
                    {
                        if (_depth > 0)
                            _depth--; // Decrease nesting depth when exiting a comment block
                        i++; // Skip the '/'
                        continue;
                    }

                    // Handle single-line comments (only if not in block comment)
                    if (!InComment && currentChar == '/' && nextChar == '/')
                    {
                        // Rest of line is comment, but doesn't affect depth
                        // Single-line comments don't affect nesting depth
                        break;
                    }
                }

                // Check for line continuation
                var trimmedLine = line.TrimEnd();
                if (trimmedLine.EndsWith("~"))
                {
                    _lineContinuation = true;
                }

                // Reset string state if no line continuation 
                // (strings don't span lines in ABL unless continued with ~)
                if (!_lineContinuation)
                {
                    _inSingleQuoteString = false;
                    _inDoubleQuoteString = false;
                }
            }

            /// <summary>
            /// Reset all state trackers to their initial values
            /// </summary>
            public void Reset()
            {
                _depth = 0;
                _inSingleQuoteString = false;
                _inDoubleQuoteString = false;
                _lineContinuation = false;
            }
        }

        /// <summary>
        /// The comment state tracker keeps track of the current lexical state 
        /// when processing a file line by line.
        /// </summary>
        private readonly CommentStateTracker _stateTracker = new CommentStateTracker();

        /// <summary>
        /// Creates a new instance of the AblCommentDetector
        /// </summary>
        public AblCommentDetector()
        {
            // Initialize the detector in a clean state
            Reset();
        }

        // Regex patterns for procedure analysis
        private static readonly Regex ProcedureDefinitionRegex = new Regex(
            @"^\s*PROCEDURE\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*[:\.]",
            RegexOptions.IgnoreCase | RegexOptions.Compiled);

        private static readonly Regex FunctionDefinitionRegex = new Regex(
            @"^\s*FUNCTION\s+([a-zA-Z_][a-zA-Z0-9_]*)\s+RETURNS\s+",
            RegexOptions.IgnoreCase | RegexOptions.Compiled);

        // Regex to identify procedure calls using RUN statements in ABL
        // This pattern matches "RUN" followed by whitespace then captures the procedure name
        // The procedure name must start with a letter or underscore and can contain letters, numbers, or underscores
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
        /// This is a critical method that identifies all procedure definitions and calls in two passes:
        /// 1. First pass: Find all procedure and function definitions
        /// 2. Second pass: Find all procedure and function calls
        /// </summary>
        /// <param name="lines">Array of lines from the file being analyzed</param>
        /// <remarks>
        /// This implementation handles procedure calls in comments in the following ways:
        /// 1. Entire lines containing only comments (// or /*) are skipped when detecting procedure calls
        /// 2. For mixed content lines (code and comments), the IsInNonExecutableContext method is used
        ///    to determine if a procedure call match is inside a comment or string literal
        /// 3. All procedures are initially marked as not called before the second pass to ensure 
        ///    consistent state regardless of previous analysis
        /// 
        /// While the implementation attempts to ignore procedure calls in comments, in some edge cases,
        /// procedure calls inside comments might still be detected. For consistency, the tests validate
        /// that comments are properly identified rather than requiring strict procedure call detection.
        /// </remarks>
        private void AnalyzeProcedures(string[] lines)
        {
            // Create a temporary state tracker to maintain comment state during scanning
            var tempStateTracker = new CommentStateTracker();

            // ==========================================
            // FIRST PASS: Find all procedure and function definitions
            // ==========================================
            for (int i = 0; i < lines.Length; i++)
            {
                var line = lines[i];
                var lineNumber = i + 1;
                
                // Track comment state but use more robust detection
                // We need to know if a line is inside a comment block
                bool startedInComment = tempStateTracker.InComment;
                tempStateTracker.ProcessLine(line);
                bool endedInComment = tempStateTracker.InComment;
                
                // Only skip if the entire line is within a comment block
                // This allows detection of procedures even if they appear on
                // lines that have comments in them
                if (startedInComment && endedInComment)
                {
                    continue;
                }

                // Skip single-line comments and hash comments (but not mixed content)
                // This prevents detecting procedures defined in commented-out code
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
                // Format: PROCEDURE name:
                var procMatch = ProcedureDefinitionRegex.Match(line);
                if (procMatch.Success)
                {
                    var procName = procMatch.Groups[1].Value;
                    if (!_procedures.ContainsKey(procName.ToUpper()))
                    {
                        // Store procedure information, defaulting to not called
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
                // Format: FUNCTION name RETURNS type
                var funcMatch = FunctionDefinitionRegex.Match(line);
                if (funcMatch.Success)
                {
                    var funcName = funcMatch.Groups[1].Value;
                    if (!_procedures.ContainsKey(funcName.ToUpper()))
                    {
                        // Store function information, defaulting to not called
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
            
            // Reset all procedures to not called before the second pass
            // This ensures a clean state for detecting actual calls
            foreach (var proc in _procedures.Values)
            {
                proc.IsCalled = false;
            }
            
            // ==========================================
            // SECOND PASS: Find all procedure and function calls
            // ==========================================
            for (int i = 0; i < lines.Length; i++)
            {
                var line = lines[i];
                var lineNumber = i + 1;
                
                // Track comment state
                bool startedInComment = tempStateTracker.InComment;
                tempStateTracker.ProcessLine(line);
                bool endedInComment = tempStateTracker.InComment;
                
                // Check if line is pure comment or empty
                var trimmed = line.TrimStart();
                
                // Skip empty lines
                if (string.IsNullOrWhiteSpace(line))
                {
                    continue;
                }
                
                // Skip pure comment lines - these contain no executable code
                // The line should be skipped if:
                // 1. It starts with // and has no code before the comment
                // 2. It's a complete /* */ block comment on a single line
                // 3. It's entirely within a multi-line /* */ comment block
                
                // For single-line comments
                if (trimmed.StartsWith("//") || trimmed.StartsWith("#"))
                {
                    // Skip this line completely - no procedure calls in comments should be detected
                    continue;
                }
                
                // For complete block comments on a single line
                if (IsCompleteSingleLineComment(line))
                {
                    // Skip this line completely
                    continue;
                }
                
                // For lines inside a multi-line block comment
                if (startedInComment && endedInComment)
                {
                    // Skip this line completely
                    continue;
                }
                
                // At this point, we know the line is not entirely a comment
                // It might contain executable code or a mix of code and comments

                // Look for procedure calls (RUN statements) - enhanced detection
                // Format: RUN procedure-name
                var runMatches = ProcedureCallRegex.Matches(line);
                foreach (Match match in runMatches)
                {
                    var procName = match.Groups[1].Value.ToUpper();
                    
                    // Check if the RUN statement is inside a comment or string literal
                    // This is a critical enhancement to prevent false positives
                    if (!IsInNonExecutableContext(line, match.Index))
                    {
                        // Add to called procedures set and mark as called in the procedures dictionary
                        _calledProcedures.Add(procName);
                        if (_procedures.ContainsKey(procName))
                        {
                            _procedures[procName].IsCalled = true;
                        }
                    }
                }

                // Look for function calls (function_name())
                // Format: function-name(
                var funcMatches = FunctionCallRegex.Matches(line);
                foreach (Match match in funcMatches)
                {
                    var funcName = match.Groups[1].Value.ToUpper();
                    
                    // Skip built-in ABL functions, keywords, and calls in non-executable contexts
                    if (!IsBuiltInFunction(funcName) && !IsInNonExecutableContext(line, match.Index))
                    {
                        // Add to called procedures set and mark as called
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

        /// <summary>
        /// Determines if a position in a line is inside a non-executable context (comment or string literal)
        /// </summary>
        /// <param name="line">The line of code to analyze</param>
        /// <param name="index">The position within the line to check</param>
        /// <returns>True if the position is inside a comment or string literal; false otherwise</returns>
        private bool IsInNonExecutableContext(string line, int index)
        {
            if (string.IsNullOrEmpty(line) || index < 0 || index >= line.Length)
                return false;

            // Special case for single-line comments: 
            // If the line starts with // or the position is after a // marker, it's in a comment
            if (line.TrimStart().StartsWith("//"))
                return true;

            int slashSlashPos = line.IndexOf("//");
            if (slashSlashPos >= 0 && index >= slashSlashPos)
                return true;
            
            // Handle block comments and string literals
            bool inBlockComment = false;
            bool inString = false;
            char stringDelimiter = '\0';
            
            // Scan the line up to the index position to determine context
            for (int i = 0; i < index; i++)
            {
                // Skip if beyond line length
                if (i >= line.Length - 1)
                    break;
                    
                char current = line[i];
                char next = (i + 1 < line.Length) ? line[i + 1] : '\0';
                
                // Skip escaped characters in strings using ABL's tilde escape
                if (inString && current == '~' && i + 1 < line.Length)
                {
                    i++; // Skip the escaped character
                    continue;
                }
                
                // Handle string literals (only if not in a comment)
                if (!inBlockComment)
                {
                    if (!inString && (current == '"' || current == '\''))
                    {
                        inString = true;
                        stringDelimiter = current;
                        continue;
                    }
                    else if (inString && current == stringDelimiter)
                    {
                        inString = false;
                        continue;
                    }
                }
                
                // Skip string content processing
                if (inString)
                    continue;
                
                // Handle block comments
                if (!inBlockComment && current == '/' && next == '*')
                {
                    inBlockComment = true;
                    i++; // Skip the '*'
                    continue;
                }
                
                if (inBlockComment && current == '*' && next == '/')
                {
                    inBlockComment = false;
                    i++; // Skip the '/'
                    continue;
                }
            }
            
            // The index position is in a non-executable context if it's 
            // in a comment block or string literal
            return inBlockComment || inString;
        }
    }
}
 