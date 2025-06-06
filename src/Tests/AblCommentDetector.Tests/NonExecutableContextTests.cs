using System;
using System.IO;
using System.Reflection;
using Xunit;
using AblCommentDetector;

namespace AblCommentDetector.Tests
{
    public class NonExecutableContextTests
    {
        private readonly AblCommentDetector _detector;
        
        public NonExecutableContextTests()
        {
            _detector = new AblCommentDetector();
        }

        // We need to use reflection to test IsInNonExecutableContext since it's private
        private bool InvokeIsInNonExecutableContext(string line, int index)
        {
            Type type = typeof(AblCommentDetector);
            MethodInfo method = type.GetMethod("IsInNonExecutableContext", 
                BindingFlags.NonPublic | BindingFlags.Instance);
            
            if (method == null)
            {
                throw new InvalidOperationException("IsInNonExecutableContext method not found. The method name might have changed.");
            }
            
            return (bool)method.Invoke(_detector, new object[] { line, index });
        }

        [Theory]
        [InlineData("RUN MyProc.", 0, false)]  // Start of line, not in comment
        [InlineData("  RUN MyProc.", 2, false)]  // After whitespace, not in comment
        [InlineData("/* RUN MyProc. */", 3, true)]  // Inside block comment
        [InlineData("DISPLAY. /* RUN MyProc. */", 13, true)]  // Inside block comment after code
        [InlineData("// RUN MyProc.", 3, true)]  // Inside single-line comment
        [InlineData("DISPLAY. // RUN MyProc.", 13, true)]  // Inside single-line comment after code
        [InlineData("DISPLAY \"RUN MyProc.\"", 10, true)]  // Inside string literal
        [InlineData("DISPLAY 'RUN MyProc.'", 10, true)]  // Inside single-quoted string
        public void IsInNonExecutableContext_ShouldDetectCorrectly(string line, int index, bool expectedResult)
        {
            // Act
            bool result = InvokeIsInNonExecutableContext(line, index);
            
            // Assert
            Assert.Equal(expectedResult, result);
        }

        [Fact]
        public void ComplexLine_WithEscapedQuotes_ShouldHandleCorrectly()
        {
            // Arrange
            string line = "DISPLAY \"String with ~\"escaped quote~\" and then more\". RUN MyProc.";
            
            // Act & Assert
            // Character after the string is complete should be in executable context
            Assert.False(InvokeIsInNonExecutableContext(line, line.IndexOf("RUN")));
            
            // Character inside the string should be in non-executable context
            Assert.True(InvokeIsInNonExecutableContext(line, line.IndexOf("escaped")));
        }

        [Fact]
        public void ComplexLine_WithMultipleCommentsAndStrings_ShouldHandleCorrectly()
        {
            // Arrange
            string line = "DISPLAY \"Hello\". /* Comment */ RUN MyProc. // Another comment";
            
            // Act & Assert
            // "DISPLAY" is in executable context
            Assert.False(InvokeIsInNonExecutableContext(line, 0));
            
            // Inside string is non-executable
            Assert.True(InvokeIsInNonExecutableContext(line, 10));
            
            // Inside block comment is non-executable
            Assert.True(InvokeIsInNonExecutableContext(line, 20));
            
            // "RUN" is in executable context
            Assert.False(InvokeIsInNonExecutableContext(line, line.IndexOf("RUN")));
            
            // Inside single-line comment is non-executable
            Assert.True(InvokeIsInNonExecutableContext(line, line.IndexOf("Another")));
        }

        [Fact]
        public void NestedBlockComments_ShouldBeHandledCorrectly()
        {
            // Arrange - technically ABL doesn't support nested comments, but our detector handles them
            string line = "/* Outer /* Nested */ Still outer */ RUN MyProc.";
            
            // Act & Assert
            // Inside outer comment is non-executable
            Assert.True(InvokeIsInNonExecutableContext(line, 10));
            
            // Inside nested comment is non-executable
            Assert.True(InvokeIsInNonExecutableContext(line, 15));
            
            // Still inside outer comment after nested is non-executable
            Assert.True(InvokeIsInNonExecutableContext(line, 25));
            
            // "RUN" is in executable context
            Assert.False(InvokeIsInNonExecutableContext(line, line.IndexOf("RUN")));
        }
        
        [Fact]
        public void LineContinuationInString_ShouldBeHandledCorrectly()
        {
            // Arrange
            // Using multiple lines to test with line continuation (~)
            var lines = new string[]
            {
                "DISPLAY \"First line ~",
                "Second line\". RUN MyProc."
            };
            
            // Process the first line to set up the state
            _detector.AnalyzeLine(lines[0], 1);
            
            // Act & Assert
            // Character at the beginning of second line should be in non-executable context (string)
            // due to line continuation
            string secondLine = lines[1];
            Assert.True(InvokeIsInNonExecutableContext(secondLine, 0));
            
            // "RUN" in second line should be in executable context
            Assert.False(InvokeIsInNonExecutableContext(secondLine, secondLine.IndexOf("RUN")));
        }
    }
} 