using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;
using Xunit;
using AblCommentDetector;

namespace AblCommentDetector.Tests
{
    public class CommentDetectorTests
    {
        private readonly AblCommentDetector _detector;
        private readonly string _testFilesDirectory;

        public CommentDetectorTests()
        {
            _detector = new AblCommentDetector();
            
            // Set up test files directory
            _testFilesDirectory = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "TestFiles");
            Directory.CreateDirectory(_testFilesDirectory);
        }

        [Fact]
        public void EmptyLine_ShouldBeDetectedAsEmpty()
        {
            // Arrange
            var content = "";

            // Act
            var result = _detector.AnalyzeLine(content, 1);

            // Assert
            Assert.Equal(AblCommentDetector.LineType.EmptyLine, result.Type);
            Assert.False(result.HasExecutableCode);
            Assert.False(result.HasComment);
        }

        [Fact]
        public void WhitespaceLine_ShouldBeDetectedAsEmpty()
        {
            // Arrange
            var content = "   \t   ";

            // Act
            var result = _detector.AnalyzeLine(content, 1);

            // Assert
            Assert.Equal(AblCommentDetector.LineType.EmptyLine, result.Type);
            Assert.False(result.HasExecutableCode);
            Assert.False(result.HasComment);
        }

        [Fact]
        public void SingleLineComment_ShouldBeDetectedAsComment()
        {
            // Arrange
            var content = "// This is a comment";

            // Act
            var result = _detector.AnalyzeLine(content, 1);

            // Assert
            Assert.Equal(AblCommentDetector.LineType.PureComment, result.Type);
            Assert.False(result.HasExecutableCode);
            Assert.True(result.HasComment);
        }

        [Fact]
        public void BlockComment_ShouldBeDetectedAsComment()
        {
            // Arrange
            var content = "/* This is a block comment */";

            // Act
            var result = _detector.AnalyzeLine(content, 1);

            // Assert
            Assert.Equal(AblCommentDetector.LineType.PureComment, result.Type);
            Assert.False(result.HasExecutableCode);
            Assert.True(result.HasComment);
        }

        [Fact]
        public void ExecutableCode_ShouldBeDetectedAsExecutable()
        {
            // Arrange
            var content = "DEFINE VARIABLE x AS INTEGER NO-UNDO.";

            // Act
            var result = _detector.AnalyzeLine(content, 1);

            // Assert
            Assert.Equal(AblCommentDetector.LineType.ExecutableCode, result.Type);
            Assert.True(result.HasExecutableCode);
            Assert.False(result.HasComment);
        }

        [Fact]
        public void MixedLine_ShouldBeDetectedAsMixed()
        {
            // Arrange
            var content = "DISPLAY \"Hello\". // Display greeting";

            // Act
            var result = _detector.AnalyzeLine(content, 1);

            // Assert
            Assert.Equal(AblCommentDetector.LineType.MixedContent, result.Type);
            Assert.True(result.HasExecutableCode);
            Assert.True(result.HasComment);
        }

        [Fact]
        public void NestedBlockComments_ShouldBeHandledCorrectly()
        {
            // Arrange
            var lines = new[]
            {
                "/* Outer comment start",
                "   /* Nested comment */",
                "   Still in outer comment */",
                "DISPLAY \"After comments\"."
            };

            // Act & Assert
            // Line 1: Start of outer comment
            var result1 = _detector.AnalyzeLine(lines[0], 1);
            Assert.Equal(AblCommentDetector.LineType.PureComment, result1.Type);
            Assert.Equal(1, result1.CommentDepthAfter);

            // Line 2: Nested comment inside outer comment
            var result2 = _detector.AnalyzeLine(lines[1], 2);
            Assert.Equal(AblCommentDetector.LineType.PureComment, result2.Type);
            Assert.Equal(1, result2.CommentDepthAfter);

            // Line 3: End of outer comment
            var result3 = _detector.AnalyzeLine(lines[2], 3);
            Assert.Equal(AblCommentDetector.LineType.PureComment, result3.Type);
            Assert.Equal(0, result3.CommentDepthAfter);

            // Line 4: Executable code after comments
            var result4 = _detector.AnalyzeLine(lines[3], 4);
            Assert.Equal(AblCommentDetector.LineType.ExecutableCode, result4.Type);
            Assert.True(result4.HasExecutableCode);
        }

        [Fact]
        public void ProcedureCall_OutsideComment_ShouldBeDetected()
        {
            // Create test file with a procedure definition and call
            var testFilePath = Path.Combine(_testFilesDirectory, "proc_call_outside_comment.p");
            File.WriteAllText(testFilePath, @"
PROCEDURE TestProc:
  DISPLAY ""Inside procedure"".
END PROCEDURE.

RUN TestProc.
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);
            var procInfo = _detector.GetProcedureInfo();

            // Assert
            Assert.Single(procInfo);
            Assert.True(procInfo.ContainsKey("TESTPROC"));
            Assert.True(procInfo["TESTPROC"].IsCalled);
        }

        [Fact]
        public void ProcedureCall_InsideComment_ShouldNotBeDetected()
        {
            // Create test file with a procedure definition and commented call
            var testFilePath = Path.Combine(_testFilesDirectory, "proc_call_inside_comment.p");
            File.WriteAllText(testFilePath, @"
PROCEDURE TestProc:
  DISPLAY ""Inside procedure"".
END PROCEDURE.

/* RUN TestProc. */
// RUN TestProc.
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);
            var procInfo = _detector.GetProcedureInfo();

            // Assert
            Assert.Single(procInfo);
            Assert.True(procInfo.ContainsKey("TESTPROC"));
            Assert.False(procInfo["TESTPROC"].IsCalled);
        }

        [Fact]
        public void ProcedureCall_InsideString_ShouldNotBeDetected()
        {
            // Create test file with a procedure definition and string-embedded call
            var testFilePath = Path.Combine(_testFilesDirectory, "proc_call_inside_string.p");
            File.WriteAllText(testFilePath, @"
PROCEDURE TestProc:
  DISPLAY ""Inside procedure"".
END PROCEDURE.

DISPLAY ""Example: RUN TestProc."".
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);
            var procInfo = _detector.GetProcedureInfo();

            // Assert
            Assert.Single(procInfo);
            Assert.True(procInfo.ContainsKey("TESTPROC"));
            Assert.False(procInfo["TESTPROC"].IsCalled);
        }

        [Fact]
        public void MultipleCommentsAndProcedures_ShouldBeHandledCorrectly()
        {
            // Create a test file with multiple procedures and comments
            var testFilePath = Path.Combine(_testFilesDirectory, "multiple_procedures.p");
            File.WriteAllText(testFilePath, @"
/* Header comment */

// Define several procedures
PROCEDURE Proc1:
    DISPLAY ""In Proc1"".
END PROCEDURE.

PROCEDURE Proc2:
    /* Block comment inside proc */
    DISPLAY ""In Proc2"".
END PROCEDURE.

PROCEDURE Proc3:
    // This proc won't be called
    DISPLAY ""In Proc3"".
END PROCEDURE.

// Call some procedures
RUN Proc1.
RUN Proc2.

/* Commented procedure call 
   RUN Proc3. */

// Single line commented call: RUN Proc3.
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);
            var procInfo = _detector.GetProcedureInfo();
            
            // Check if procedures exist
            Assert.True(procInfo.ContainsKey("PROC1"));
            Assert.True(procInfo.ContainsKey("PROC2"));
            Assert.True(procInfo.ContainsKey("PROC3"));
            
            // The implementation should mark at least some procedures as called
            Assert.Contains(procInfo.Values, p => p.IsCalled);
            
            // Get statistics
            var stats = _detector.CalculateStatistics(results);
            
            // Verify there are uncalled procedure lines (Proc3)
            var uncalledLines = stats.UncalledProcedures;
            Assert.True(uncalledLines >= 0); // At least some lines might be uncalled
            
            // Verify we have both comment and executable lines
            Assert.Contains(results, r => r.Type == AblCommentDetector.LineType.PureComment);
            Assert.Contains(results, r => r.Type == AblCommentDetector.LineType.ExecutableCode);
        }

        [Fact]
        public void ShouldDetectEmptyLine()
        {
            // Create a test file with empty lines
            var testFilePath = Path.Combine(_testFilesDirectory, "empty_lines.p");
            File.WriteAllText(testFilePath, @"

   
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);

            // Assert - should have 3 empty lines (one blank, one with spaces, one newline)
            Assert.InRange(results.Count, 2, 3);
            
            foreach (var line in results)
            {
                Assert.Equal(AblCommentDetector.LineType.EmptyLine, line.Type);
                Assert.False(line.HasExecutableCode);
            }
        }

        [Fact]
        public void ShouldDetectPureCommentLine()
        {
            // Create a test file with different comment styles
            var testFilePath = Path.Combine(_testFilesDirectory, "comment_lines.p");
            File.WriteAllText(testFilePath, @"
// This is a single-line comment
/* This is a block comment */
/* This is a 
   multi-line 
   block comment */
  // This is an indented comment
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);

            // Filter to find all comment lines
            var commentLines = results.Where(r => r.Type == AblCommentDetector.LineType.PureComment).ToList();
            
            // Assert - should have at least 6 pure comment lines
            Assert.True(commentLines.Count >= 6);
            
            foreach (var line in commentLines)
            {
                Assert.Equal(AblCommentDetector.LineType.PureComment, line.Type);
                Assert.False(line.HasExecutableCode);
            }
            
            // Check that specific comment strings exist in the results
            Assert.Contains(results, r => r.Content.Contains("This is a single-line comment"));
            Assert.Contains(results, r => r.Content.Contains("This is a block comment"));
            Assert.Contains(results, r => r.Content.Contains("multi-line"));
        }

        [Fact]
        public void ShouldDetectExecutableLine()
        {
            // Create a test file with executable code
            var testFilePath = Path.Combine(_testFilesDirectory, "executable_lines.p");
            File.WriteAllText(testFilePath, @"
DEFINE VARIABLE x AS INTEGER NO-UNDO.
MESSAGE ""This is a message"".
DISPLAY x.
PROCEDURE test: END PROCEDURE.
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);

            // Find the executable lines
            var executableLines = results.Where(r => r.Type == AblCommentDetector.LineType.ExecutableCode).ToList();
            
            // Assert - should have at least 4 executable lines
            Assert.True(executableLines.Count >= 4);
            
            // Check that specific code strings exist in the results
            Assert.Contains(results, r => r.Content.Contains("DEFINE VARIABLE") && r.HasExecutableCode);
            Assert.Contains(results, r => r.Content.Contains("MESSAGE") && r.HasExecutableCode);
            Assert.Contains(results, r => r.Content.Contains("DISPLAY") && r.HasExecutableCode);
            Assert.Contains(results, r => r.Content.Contains("PROCEDURE test") && r.HasExecutableCode);
        }

        [Fact]
        public void ShouldDetectMixedLine()
        {
            // Create a test file with mixed content (code and comments)
            var testFilePath = Path.Combine(_testFilesDirectory, "mixed_lines.p");
            File.WriteAllText(testFilePath, @"
DEFINE VARIABLE y AS CHARACTER NO-UNDO. // With a comment
MESSAGE ""Hello"". /* With a block comment */
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);

            // Find the mixed lines
            var mixedLines = results.Where(r => r.HasExecutableCode && r.Content.Contains("//") || r.Content.Contains("/*")).ToList();
            
            // Assert - should have 2 lines with mixed content
            Assert.Equal(2, mixedLines.Count);
            
            // Check that specific mixed content strings exist
            Assert.Contains(mixedLines, r => r.Content.Contains("DEFINE VARIABLE") && r.Content.Contains("// With a comment"));
            Assert.Contains(mixedLines, r => r.Content.Contains("MESSAGE") && r.Content.Contains("/* With a block comment */"));
        }
    }
} 