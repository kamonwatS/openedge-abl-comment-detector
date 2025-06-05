using System;
using System.IO;
using System.Linq;

namespace AblCommentDetector
{
    /// <summary>
    /// Console application for testing the OpenEdge ABL comment detector
    /// </summary>
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("OpenEdge ABL Comment Detection Tool");
            Console.WriteLine("===================================");
            Console.WriteLine("Based on grammar patterns from https://github.com/ezequielgandolfi/openedge-zext.git");
            Console.WriteLine();

            if (args.Length != 1)
            {
                Console.WriteLine("Usage: AblCommentDetector <input-file>");
                Console.WriteLine("Output will be saved as: <input-file>-modified.w");
                Console.WriteLine();
                Console.WriteLine("Examples:");
                Console.WriteLine("  AblCommentDetector WRSBQ7072.W");
                Console.WriteLine("  -> Output: WRSBQ7072-modified.w");
                return;
            }

            string inputFile = args[0];
            
            // Generate output filename: remove extension, add "-modified.w"
            string outputFile = Path.GetFileNameWithoutExtension(inputFile) + "-modified.w";

            try
            {
                Console.WriteLine($"Analyzing file: {inputFile}");
                Console.WriteLine();

                var detector = new AblCommentDetector();
                var results = detector.AnalyzeFile(inputFile);
                
                // Calculate and display statistics
                var stats = detector.CalculateStatistics(results);
                
                Console.WriteLine("Analysis Summary:");
                Console.WriteLine($"  Total lines: {results.Count}");
                Console.WriteLine($"  Executable code: {stats.ExecutableLines} lines ({stats.ExecutablePercentage:F1}%)");
                Console.WriteLine($"  Pure comments: {stats.CommentLines} lines ({stats.CommentPercentage:F1}%)");
                Console.WriteLine($"  Mixed content: {stats.MixedLines} lines ({stats.MixedPercentage:F1}%)");
                Console.WriteLine($"  Empty lines: {stats.EmptyLines} lines ({stats.EmptyPercentage:F1}%)");
                Console.WriteLine();

                // Generate the output file
                detector.GenerateReport(inputFile, outputFile);
                Console.WriteLine($"Detailed report saved to: {outputFile}");
                Console.WriteLine();

                // Show some example classifications
                Console.WriteLine("Example classifications:");
                Console.WriteLine("========================");
                Console.WriteLine();

                // Show executable code examples
                var executableExamples = results.Where(r => r.Type == AblCommentDetector.LineType.ExecutableCode).Take(3);
                if (executableExamples.Any())
                {
                    Console.WriteLine("Executable Code Examples:");
                    foreach (var example in executableExamples)
                    {
                        Console.WriteLine($"  Line {example.LineNumber,5}: {example.Content.Trim().Substring(0, Math.Min(80, example.Content.Trim().Length))}...");
                    }
                    Console.WriteLine();
                }

                // Show comment examples
                var commentExamples = results.Where(r => r.Type == AblCommentDetector.LineType.PureComment).Take(3);
                if (commentExamples.Any())
                {
                    Console.WriteLine("Pure Comment Examples:");
                    foreach (var example in commentExamples)
                    {
                        Console.WriteLine($"  Line {example.LineNumber,5}: {example.Content.Trim()}");
                    }
                    Console.WriteLine();
                }

                // Show mixed content examples
                var mixedExamples = results.Where(r => r.Type == AblCommentDetector.LineType.MixedContent).Take(3);
                if (mixedExamples.Any())
                {
                    Console.WriteLine("Mixed Content Examples (treated as executable):");
                    foreach (var example in mixedExamples)
                    {
                        Console.WriteLine($"  Line {example.LineNumber,5}: {example.Content.Trim().Substring(0, Math.Min(80, example.Content.Trim().Length))}...");
                    }
                    Console.WriteLine();
                }

                Console.WriteLine("Analysis complete!");
            }
            catch (FileNotFoundException)
            {
                Console.WriteLine($"Error: File '{inputFile}' not found.");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Error: {ex.Message}");
            }
        }
    }
} 