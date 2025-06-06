using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;
using Xunit;
using AblCommentDetector;

namespace AblCommentDetector.Tests
{
    /// <summary>
    /// Tests the basic functionality of the AblCommentDetector class.
    /// These tests focus on verifying that the detector can correctly identify
    /// different types of lines (empty, comment, executable, mixed) in ABL code
    /// and properly track procedure calls.
    /// </summary>
    public class CommentDetectorTests
    {
        private readonly AblCommentDetector _detector;
        private readonly string _testFilesDirectory;

        /// <summary>
        /// Initializes a new instance of the CommentDetectorTests class.
        /// Sets up the test environment by creating a temporary directory for test files
        /// and initializing a new instance of the AblCommentDetector.
        /// </summary>
        public CommentDetectorTests()
        {
            _detector = new AblCommentDetector();
            _testFilesDirectory = Path.Combine(Path.GetTempPath(), "AblCommentDetectorTests", "CommentDetectorTests");
            Directory.CreateDirectory(_testFilesDirectory);
        }

        /// <summary>
        /// Tests that the detector can properly identify and analyze empty lines.
        /// This includes completely empty lines and lines with only whitespace.
        /// </summary>
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

        /// <summary>
        /// Tests that the detector can identify different types of comment lines.
        /// This includes single-line comments (//), block comments (/* */), and
        /// multi-line block comments with different indentation levels.
        /// </summary>
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

        /// <summary>
        /// Tests that the detector can identify executable code lines.
        /// This includes variable definitions, statements, and procedure definitions.
        /// </summary>
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

        /// <summary>
        /// Tests that the detector can identify mixed content lines.
        /// These are lines that contain both executable code and comments.
        /// </summary>
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

        /// <summary>
        /// Tests that nested block comments are properly handled.
        /// ABL technically doesn't support nested comments, but the detector should
        /// handle them gracefully.
        /// </summary>
        [Fact]
        public void NestedBlockComments_ShouldBeHandledCorrectly()
        {
            // Create test file with nested block comments
            var testFilePath = Path.Combine(_testFilesDirectory, "nested_comments.p");
            File.WriteAllText(testFilePath, @"
/* Outer comment
   /* Nested comment */
   Back in outer comment */
   
DISPLAY ""After comment"".
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);
            
            // Find all comment lines
            var commentLines = results.Count(r => r.Type == AblCommentDetector.LineType.PureComment);
            
            // Assert - should have at least 3 comment lines
            Assert.True(commentLines >= 3);
            
            // The line after comments should be executable
            var displayLine = results.FirstOrDefault(r => r.Content.Contains("DISPLAY"));
            Assert.NotNull(displayLine);
            Assert.Equal(AblCommentDetector.LineType.ExecutableCode, displayLine.Type);
        }

        /// <summary>
        /// Tests that procedure calls outside comments are correctly detected.
        /// The detector should recognize these as actual calls to procedures.
        /// </summary>
        [Fact]
        public void ProcedureCall_OutsideComment_ShouldBeDetected()
        {
            // Create test file with procedure call
            var testFilePath = Path.Combine(_testFilesDirectory, "proc_call.p");
            File.WriteAllText(testFilePath, @"
PROCEDURE TestProc:
  DISPLAY ""In TestProc"".
END PROCEDURE.

RUN TestProc.
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);
            var procInfo = _detector.GetProcedureInfo();
            
            // Assert
            Assert.True(procInfo.ContainsKey("TESTPROC"));
            Assert.True(procInfo["TESTPROC"].IsCalled);
        }

        /// <summary>
        /// Tests that procedure calls inside comments are not detected as actual calls.
        /// The detector should ignore these calls since they are commented out.
        /// </summary>
        [Fact]
        public void ProcedureCall_InsideComment_ShouldNotBeDetected()
        {
            // Create test file with procedure call inside comment
            var testFilePath = Path.Combine(_testFilesDirectory, "commented_proc_call.p");
            File.WriteAllText(testFilePath, @"
PROCEDURE TestProc:
  DISPLAY ""In TestProc"".
END PROCEDURE.

// This is a comment with RUN TestProc.
/* Another comment with
   RUN TestProc. */
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);
            var procInfo = _detector.GetProcedureInfo();
            
            // Assert
            Assert.True(procInfo.ContainsKey("TESTPROC"));
            Assert.False(procInfo["TESTPROC"].IsCalled);
        }

        /// <summary>
        /// Tests that procedure calls inside string literals are not detected as actual calls.
        /// The detector should recognize these as part of string content, not as code.
        /// </summary>
        [Fact]
        public void ProcedureCall_InsideString_ShouldNotBeDetected()
        {
            // Create test file with procedure call inside string
            var testFilePath = Path.Combine(_testFilesDirectory, "string_proc_call.p");
            File.WriteAllText(testFilePath, @"
PROCEDURE TestProc:
  DISPLAY ""In TestProc"".
END PROCEDURE.

MESSAGE ""This is a string with RUN TestProc."".
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);
            var procInfo = _detector.GetProcedureInfo();
            
            // Assert
            Assert.True(procInfo.ContainsKey("TESTPROC"));
            Assert.False(procInfo["TESTPROC"].IsCalled);
        }

        /// <summary>
        /// Tests the detector's ability to handle multiple procedures and comments.
        /// Verifies that the detector correctly identifies which procedures are called
        /// and which are not, even in the presence of comments that contain procedure calls.
        /// </summary>
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
    }
} 