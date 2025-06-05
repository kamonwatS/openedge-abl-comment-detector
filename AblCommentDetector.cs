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
    /// Now with proper lexical analysis for complex patterns
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

        /// <summary>
        /// Analyzes an entire file and returns results for each line
        /// </summary>
        public List<LineAnalysisResult> AnalyzeFile(string filePath)
        {
            if (!File.Exists(filePath))
                throw new FileNotFoundException($"File not found: {filePath}");

            _results.Clear();
            _stateTracker.Reset();

            var lines = File.ReadAllLines(filePath);
            
            for (int i = 0; i < lines.Length; i++)
            {
                var result = AnalyzeLine(lines[i], i + 1);
                _results.Add(result);
            }

            return _results;
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

            return result;
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
                    writer.WriteLine($"/*[COMMENT]*/                    {result.Content}");
                }
                else if (result.Type == LineType.EmptyLine)
                {
                    writer.WriteLine($"/*[BLANK]*/                      {result.Content}");
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
                int EmptyLines, double EmptyPercentage) CalculateStatistics(List<LineAnalysisResult> results)
        {
            var total = results.Count;
            var executable = results.Count(r => r.Type == LineType.ExecutableCode);
            var comments = results.Count(r => r.Type == LineType.PureComment);
            var mixed = results.Count(r => r.Type == LineType.MixedContent);
            var empty = results.Count(r => r.Type == LineType.EmptyLine);

            return (
                ExecutableLines: executable,
                ExecutablePercentage: (double)executable / total * 100,
                CommentLines: comments,
                CommentPercentage: (double)comments / total * 100,
                MixedLines: mixed,
                MixedPercentage: (double)mixed / total * 100,
                EmptyLines: empty,
                EmptyPercentage: (double)empty / total * 100
            );
        }

        /// <summary>
        /// Resets the detector state for analyzing a new file
        /// </summary>
        public void Reset()
        {
            _stateTracker.Reset();
            _results.Clear();
        }
    }
}
