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
            // Create test file with multiple procedures and comments
            var testFilePath = Path.Combine(_testFilesDirectory, "multiple_procs_and_comments.p");
            File.WriteAllText(testFilePath, @"
/* This is a header comment
   with multiple lines */

// Define the first procedure
PROCEDURE Proc1:
  DISPLAY ""In procedure 1"".
END PROCEDURE.

/* This procedure will be called */
PROCEDURE Proc2:
  DISPLAY ""In procedure 2"".
END PROCEDURE.

// This procedure will not be called
PROCEDURE Proc3:
  DISPLAY ""In procedure 3"".
  /* Nested comment in uncalled procedure
     RUN Proc1. */
END PROCEDURE.

/* Commented call
RUN Proc1. */

// Another commented call: RUN Proc3.

RUN Proc2. // Call procedure 2
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);
            var procInfo = _detector.GetProcedureInfo();
            var stats = _detector.CalculateStatistics(results);

            // Assert
            Assert.Equal(3, procInfo.Count);
            Assert.False(procInfo["PROC1"].IsCalled);
            Assert.True(procInfo["PROC2"].IsCalled);
            Assert.False(procInfo["PROC3"].IsCalled);
            
            // Check that comments and blank lines are correctly identified
            var commentLines = results.Count(r => r.Type == AblCommentDetector.LineType.PureComment);
            var blankLines = results.Count(r => r.Type == AblCommentDetector.LineType.EmptyLine);
            var uncalledLines = results.Count(r => r.IsUncalledProcedure);
            
            Assert.True(commentLines > 0);
            Assert.True(blankLines > 0);
            Assert.True(uncalledLines > 0);
        }
    }
} 