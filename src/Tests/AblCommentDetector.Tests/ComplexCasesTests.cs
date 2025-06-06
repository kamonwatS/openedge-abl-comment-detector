using System;
using System.IO;
using System.Linq;
using Xunit;
using AblCommentDetector;

namespace AblCommentDetector.Tests
{
    public class ComplexCasesTests
    {
        private readonly AblCommentDetector _detector;
        private readonly string _testFilesDirectory;

        public ComplexCasesTests()
        {
            _detector = new AblCommentDetector();
            
            // Set up test files directory
            _testFilesDirectory = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "TestFiles");
            Directory.CreateDirectory(_testFilesDirectory);
        }

        [Fact]
        public void NestedComments_ShouldBeHandledCorrectly()
        {
            // Create a test file with nested comments
            var testFilePath = Path.Combine(_testFilesDirectory, "nested_comments.p");
            File.WriteAllText(testFilePath, @"
/* Level 1 comment start
   /* Level 2 comment */
   /* Level 2 comment with RUN NestedProc. */
   Level 1 comment continues
   /* Another level 2 */
   Level 1 comment end */

PROCEDURE NestedProc:
  DISPLAY ""Inside nested proc"".
END PROCEDURE.

RUN NestedProc. /* This call should be counted */
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);
            var procInfo = _detector.GetProcedureInfo();
            
            // Assert
            Assert.True(procInfo["NESTEDPROC"].IsCalled); // Procedure should be marked as called
            
            // The first 7 lines should be marked as comments
            for (int i = 0; i < 7; i++)
            {
                Assert.Equal(AblCommentDetector.LineType.PureComment, results[i].Type);
            }
            
            // The RUN statement should be executable
            Assert.Equal(AblCommentDetector.LineType.MixedContent, results[12].Type);
            Assert.True(results[12].HasExecutableCode);
        }

        [Fact]
        public void LineContinuation_ShouldBeHandledCorrectly()
        {
            // Create a test file with line continuations
            var testFilePath = Path.Combine(_testFilesDirectory, "line_continuation.p");
            File.WriteAllText(testFilePath, @"
DEFINE VARIABLE longVar AS CHARACTER NO-UNDO.

/* Comment with continuation ~
   that continues here */

PROCEDURE ContProc:
  DISPLAY ""This string has a continuation ~
           and continues here"".
  
  longVar = ""Another string with ~
             continuation"".
END PROCEDURE.

/* RUN ContProc ~
   shouldn't call the procedure */

DISPLAY ""RUN ContProc ~
         is in a string"".

RUN ContProc. // This should call the procedure
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);
            var procInfo = _detector.GetProcedureInfo();
            
            // Assert
            Assert.True(procInfo["CONTPROC"].IsCalled); // Procedure should be marked as called
            
            // Check that DISPLAY with continued string is one executable statement
            var displayLine = results.First(r => r.Content.Contains("This string has a continuation"));
            Assert.Equal(AblCommentDetector.LineType.ExecutableCode, displayLine.Type);
            
            // Check that comment with continuation is detected as comment
            var commentLine1 = results.First(r => r.Content.Contains("Comment with continuation"));
            var commentLine2 = results.First(r => r.Content.Contains("that continues here"));
            Assert.Equal(AblCommentDetector.LineType.PureComment, commentLine1.Type);
            Assert.Equal(AblCommentDetector.LineType.PureComment, commentLine2.Type);
            
            // Check that RUN in a continued comment doesn't count as a call
            var commentRunLine = results.First(r => r.Content.Contains("RUN ContProc ~"));
            Assert.Equal(AblCommentDetector.LineType.PureComment, commentRunLine.Type);
        }

        [Fact]
        public void MixedStringAndCommentCases_ShouldBeHandledCorrectly()
        {
            // Create a test file with mixed string and comment cases
            var testFilePath = Path.Combine(_testFilesDirectory, "mixed_cases.p");
            File.WriteAllText(testFilePath, @"
PROCEDURE MixedCase:
  DISPLAY ""String with /* comment-like */ content"".
  DISPLAY ""String with // comment-like content"".
  DISPLAY ""String with ~"" quote inside ~"" it"".
END PROCEDURE.

PROCEDURE EscapedQuote:
  DISPLAY ""Escaped ~"" quote"".
END PROCEDURE.

/* Comment with ""string-like"" content */
/* Comment with 'string-like' content */

// Comment with ""string inside"" and RUN MixedCase.
// Comment with 'string inside' and RUN EscapedQuote.

MESSAGE ""This /* is not a comment */ but a string"".
MESSAGE ""This // is not a comment but a string"".

/* This is /* a nested comment with a ""string"" */ */

RUN MixedCase.
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);
            var procInfo = _detector.GetProcedureInfo();
            
            // Assert
            Assert.Equal(2, procInfo.Count);
            Assert.True(procInfo["MIXEDCASE"].IsCalled);
            Assert.False(procInfo["ESCAPEDQUOTE"].IsCalled);
            
            // Verify line classifications
            var stringWithCommentLike = results.FirstOrDefault(r => r.Content.Contains("String with /* comment-like"));
            Assert.Equal(AblCommentDetector.LineType.ExecutableCode, stringWithCommentLike.Type);
            
            var commentWithStringLike = results.FirstOrDefault(r => r.Content.Contains("Comment with \"string-like\""));
            Assert.Equal(AblCommentDetector.LineType.PureComment, commentWithStringLike.Type);
            
            var messageWithCommentLike = results.FirstOrDefault(r => r.Content.Contains("This /* is not a comment"));
            Assert.Equal(AblCommentDetector.LineType.ExecutableCode, messageWithCommentLike.Type);
        }

        [Fact]
        public void ComplexExample_ShouldHandleAllCases()
        {
            // Create a comprehensive test file with all complex cases
            var testFilePath = Path.Combine(_testFilesDirectory, "complex_example.p");
            File.WriteAllText(testFilePath, @"
/* This is a complex example with multiple
   comment styles and procedures */

// Procedure definitions
PROCEDURE Proc1:
  /* Nested comment inside procedure
     with multiple lines */
  DISPLAY ""In Proc1"".
  MESSAGE ""String with ~"" escaped quotes ~"" and RUN commands"".
  // RUN Proc2. (this is commented out)
END PROCEDURE.

PROCEDURE Proc2:
  DISPLAY ""In Proc2"".
  
  // Continued string
  MESSAGE ""String that ~
          continues on ~
          multiple lines"".
END PROCEDURE.

PROCEDURE Proc3:
  DISPLAY ""In Proc3"".
  
  /* Comment with code-like content:
     MESSAGE ""This looks like code"".
     RUN Proc1.
  */
  
  DISPLAY /* inline comment */ ""After comment"".
END PROCEDURE.

// Call some of the procedures
RUN Proc1.

/* Commented call:
   RUN Proc2. */

// Another commented call: RUN Proc3.

/* Block with nested comments:
   /* First nested level with a call that should be ignored */
   /* RUN Proc2. */ 
   Still in outer comment
*/

DISPLAY ""String with a comment: /* this is not a real comment */"".
DISPLAY ""String with code: RUN Proc2. (not a real call)"".

// This is the only real call to Proc2
RUN Proc2. // End of file
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);
            var procInfo = _detector.GetProcedureInfo();
            var stats = _detector.CalculateStatistics(results);
            
            // Assert
            Assert.Equal(3, procInfo.Count);
            Assert.True(procInfo["PROC1"].IsCalled);
            Assert.True(procInfo["PROC2"].IsCalled);
            Assert.False(procInfo["PROC3"].IsCalled);
            
            // Check that stats add up to the total number of lines
            int totalLines = results.Count;
            int verifiedLines = stats.ExecutableLines + stats.CommentLines + 
                               stats.EmptyLines + stats.UncalledProcedures + stats.MixedLines;
            Assert.Equal(totalLines, verifiedLines);
            
            // Make sure the call on the last line is detected
            var lastCallLine = results.FirstOrDefault(r => r.Content.Trim().StartsWith("RUN Proc2."));
            Assert.NotNull(lastCallLine);
            Assert.True(lastCallLine.HasExecutableCode);
        }
    }
} 