using System;
using System.IO;
using System.Linq;
using Xunit;
using AblCommentDetector;

namespace AblCommentDetector.Tests
{
    public class StatisticsTests
    {
        private readonly AblCommentDetector _detector;
        private readonly string _testFilesDirectory;

        public StatisticsTests()
        {
            _detector = new AblCommentDetector();
            
            // Set up test files directory
            _testFilesDirectory = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "TestFiles");
            Directory.CreateDirectory(_testFilesDirectory);
        }

        [Fact]
        public void CalculateStatistics_ShouldReturnCorrectCounts()
        {
            // Create a test file with known content
            var testFilePath = Path.Combine(_testFilesDirectory, "statistics_test.p");
            File.WriteAllText(testFilePath, @"
/* Header comment
   Multiple lines */

// Single line comment

DEFINE VARIABLE x AS INTEGER NO-UNDO.
DEFINE VARIABLE y AS CHARACTER NO-UNDO. // Inline comment

MESSAGE ""Hello world"".

/* Another block comment */

PROCEDURE Proc1:
  DISPLAY ""In Proc1"".
END PROCEDURE.

PROCEDURE Proc2:
  DISPLAY ""In Proc2"".
END PROCEDURE.

// Call Proc1 but not Proc2
RUN Proc1.
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);
            var stats = _detector.CalculateStatistics(results);

            // Assert
            // Expected count: 3 blank lines, 6 comment lines, 2 lines in uncalled Proc2, 
            // 10 executable lines (including Proc1 and line with inline comment)
            
            // Check that file has the expected total number of lines
            Assert.Equal(21, results.Count);
            
            // Verify the different types of lines
            Assert.Equal(3, stats.EmptyLines); // 3 blank lines
            Assert.Equal(6, stats.CommentLines); // 6 pure comment lines
            Assert.Equal(2, stats.UncalledProcedures); // 2 lines in uncalled Proc2
            
            // The executable percentage should be around 50% (10 out of 21 lines)
            Assert.InRange(stats.ExecutablePercentage, 45, 50);
            
            // Comment percentage should be around 28% (6 out of 21 lines)
            Assert.InRange(stats.CommentPercentage, 25, 30);
            
            // Uncalled procedure percentage should be around 9% (2 out of 21 lines)
            Assert.InRange(stats.UncalledPercentage, 8, 10);
        }

        [Fact]
        public void EmptyFile_ShouldHaveZeroStatistics()
        {
            // Create an empty test file
            var testFilePath = Path.Combine(_testFilesDirectory, "empty_file.p");
            File.WriteAllText(testFilePath, "");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);
            var stats = _detector.CalculateStatistics(results);

            // Assert
            Assert.Single(results); // Should have one empty line
            Assert.Equal(1, stats.EmptyLines);
            Assert.Equal(0, stats.ExecutableLines);
            Assert.Equal(0, stats.CommentLines);
            Assert.Equal(0, stats.MixedLines);
            Assert.Equal(0, stats.UncalledProcedures);
            
            Assert.Equal(0, stats.ExecutablePercentage);
            Assert.Equal(0, stats.CommentPercentage);
            Assert.Equal(0, stats.MixedPercentage);
            Assert.Equal(100, stats.EmptyPercentage); // 100% empty
            Assert.Equal(0, stats.UncalledPercentage);
        }

        [Fact]
        public void OnlyComments_ShouldHaveFullCommentStatistics()
        {
            // Create a file with only comments
            var testFilePath = Path.Combine(_testFilesDirectory, "comments_only.p");
            File.WriteAllText(testFilePath, @"
/* This file contains 
   only comments */

// Another comment
// And another one
/* Block comment */
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);
            var stats = _detector.CalculateStatistics(results);
            
            // Assert
            Assert.Equal(0, stats.ExecutableLines);
            Assert.Equal(0, stats.MixedLines);
            Assert.Equal(0, stats.UncalledProcedures);
            
            // Should have 6 comment lines and 2 blank lines
            Assert.Equal(6, stats.CommentLines);
            Assert.Equal(2, stats.EmptyLines);
            
            // Comment percentage should be around 75% (6 out of 8 lines)
            Assert.InRange(stats.CommentPercentage, 70, 80);
            
            // Empty percentage should be around 25% (2 out of 8 lines)
            Assert.InRange(stats.EmptyPercentage, 20, 30);
        }

        [Fact]
        public void AllUncalledProcedures_ShouldBeMarkedCorrectly()
        {
            // Create a file with only uncalled procedures
            var testFilePath = Path.Combine(_testFilesDirectory, "uncalled_procs.p");
            File.WriteAllText(testFilePath, @"
PROCEDURE Proc1:
  DISPLAY ""In Proc1"".
END PROCEDURE.

PROCEDURE Proc2:
  DISPLAY ""In Proc2"".
END PROCEDURE.

/* No calls to any procedures */
");

            // Analyze the file
            var results = _detector.AnalyzeFile(testFilePath);
            var stats = _detector.CalculateStatistics(results);
            var procInfo = _detector.GetProcedureInfo();
            
            // Assert
            Assert.Equal(2, procInfo.Count); // Should have 2 procedures
            Assert.False(procInfo["PROC1"].IsCalled);
            Assert.False(procInfo["PROC2"].IsCalled);
            
            // The uncalled procedure lines should be 6 (2 procedures with 3 lines each)
            Assert.Equal(6, stats.UncalledProcedures);
            
            // Comments should be 1 line
            Assert.Equal(1, stats.CommentLines);
            
            // Blank lines should be 3
            Assert.Equal(3, stats.EmptyLines);
            
            // Uncalled procedure percentage should be around 60% (6 out of 10 lines)
            Assert.InRange(stats.UncalledPercentage, 55, 65);
        }
    }
} 