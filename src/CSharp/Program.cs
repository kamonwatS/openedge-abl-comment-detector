using System;
using System.IO;
using System.Linq;
using AblCommentDetector;
using System.Collections.Generic;
using System.Reflection;

namespace AblCommentDetector
{
    /// <summary>
    /// Console application for testing the OpenEdge ABL comment detector
    /// </summary>
    class Program
    {
        static void Main(string[] args)
        {
            // Ensure root-level outputs directory exists
            // This is important for consistent output regardless of where the program is run from
            string projectRoot = GetProjectRoot();
            Directory.CreateDirectory(Path.Combine(projectRoot, "outputs"));

            Console.WriteLine("OpenEdge ABL Comment Detection Tool");
            Console.WriteLine("===================================");
            Console.WriteLine("Based on grammar patterns from https://github.com/ezequielgandolfi/openedge-zext.git");
            Console.WriteLine();

            // Display usage information if no arguments provided
            if (args.Length == 0)
            {
                Console.WriteLine("Usage:");
                Console.WriteLine("  dotnet run -- <file_path>       # Process single file");
                Console.WriteLine("  dotnet run -- <directory_path>  # Process all ABL files in directory");
                Console.WriteLine();
                Console.WriteLine("Examples:");
                Console.WriteLine("  dotnet run -- inputs/WRSBQ7072.W");
                Console.WriteLine("  dotnet run -- inputs/sample-code.p");
                Console.WriteLine("  dotnet run -- inputs/");
                return;
            }

            string inputPath = args[0];
            
            try
            {
                // Determine if we're processing a single file or a directory
                if (File.Exists(inputPath))
                {
                    ProcessSingleFile(inputPath);
                }
                else if (Directory.Exists(inputPath))
                {
                    ProcessDirectory(inputPath);
                }
                else
                {
                    Console.WriteLine($"Error: Path '{inputPath}' does not exist.");
                    return;
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Error: {ex.Message}");
            }
        }

        /// <summary>
        /// Gets the project root directory regardless of where the application is run from.
        /// This is crucial for consistent file path resolution and output generation.
        /// </summary>
        /// <returns>The absolute path to the project root directory</returns>
        static string GetProjectRoot()
        {
            // Get the directory where the executable is running
            string executableDir = AppDomain.CurrentDomain.BaseDirectory;
            
            // If running from bin directory (e.g., bin/Debug/net6.0), navigate up to project root
            if (executableDir.Contains("bin"))
            {
                // Check if we're in src/CSharp/bin or similar structure
                if (executableDir.Contains(Path.Combine("src", "CSharp", "bin")))
                {
                    // Navigate up to project root (three levels up from bin directory)
                    return Path.GetFullPath(Path.Combine(executableDir, "..", "..", ".."));
                }
                else
                {
                    // For other bin structures, just navigate two levels up
                    return Path.GetFullPath(Path.Combine(executableDir, "..", ".."));
                }
            }
            
            // If not in a bin directory, we might be in the project root already
            // Check for common project identifiers to confirm
            string currentDir = Directory.GetCurrentDirectory();
            if (Directory.Exists(Path.Combine(currentDir, "src")) && 
                Directory.Exists(Path.Combine(currentDir, "inputs")))
            {
                return currentDir;
            }
            
            // If we're in the src/CSharp directory
            if (Path.GetFileName(currentDir) == "CSharp" && 
                Path.GetFileName(Path.GetDirectoryName(currentDir)) == "src")
            {
                return Path.GetFullPath(Path.Combine(currentDir, "..", ".."));
            }
            
            // Fallback: Just use the current directory
            return currentDir;
        }

        /// <summary>
        /// Processes a single ABL file, analyzing its contents for comments and procedure calls
        /// </summary>
        /// <param name="filePath">Path to the ABL file to process</param>
        static void ProcessSingleFile(string filePath)
        {
            Console.WriteLine($"Analyzing file: {Path.GetFileName(filePath)}");
            Console.WriteLine();

            // Create detector instance and analyze the file
            var detector = new AblCommentDetector();
            var results = detector.AnalyzeFile(filePath);

            // Set up output paths relative to project root
            // This ensures consistent output locations regardless of where the tool is run from
            string projectRoot = GetProjectRoot();
            string fileName = Path.GetFileNameWithoutExtension(filePath);
            string outputDir = Path.Combine(projectRoot, "outputs", fileName);
            Directory.CreateDirectory(outputDir);

            // Copy original file to output directory for reference
            string originalOutputPath = Path.Combine(outputDir, Path.GetFileName(filePath));
            File.Copy(filePath, originalOutputPath, true);

            // Generate modified file in output directory with comment annotations
            string modifiedFileName = $"{fileName}-modified{Path.GetExtension(filePath)}";
            string modifiedOutputPath = Path.Combine(outputDir, modifiedFileName);
            detector.GenerateReport(filePath, modifiedOutputPath);

            // Display statistics and examples to the console
            DisplayStatistics(results, detector);
            DisplayExamples(results);

            Console.WriteLine($"Original file copied to: {originalOutputPath}");
            Console.WriteLine($"Modified file saved to: {modifiedOutputPath}");
            Console.WriteLine();
            
            // Generate analysis-result.txt for verification
            // This contains details about all categories of lines and procedure calls
            var stats = detector.CalculateStatistics(results);
            var procedureInfo = detector.GetProcedureInfo();
            
            var fileResult = new FileAnalysisResult
            {
                FileName = Path.GetFileName(filePath),
                OriginalLines = results.Count,
                ExecutableLines = stats.ExecutableLines + stats.MixedLines,
                CommentLines = stats.CommentLines,
                BlankLines = stats.EmptyLines,
                UncalledProcedureLines = stats.UncalledProcedures,
                CalledProcedures = procedureInfo.Values.Where(p => p.IsCalled).Select(p => p.Name).ToList(),
                UncalledProcedures = procedureInfo.Values.Where(p => !p.IsCalled).Select(p => p.Name).ToList()
            };
            
            var fileResults = new List<FileAnalysisResult> { fileResult };
            GenerateAnalysisResultFile(fileResults);
            
            Console.WriteLine("Analysis complete!");
        }

        static void ProcessDirectory(string directoryPath)
        {
            // Get all ABL files in directory
            string[] ablExtensions = { "*.w", "*.W", "*.p", "*.P", "*.i", "*.I" };
            var ablFiles = ablExtensions
                .SelectMany(ext => Directory.GetFiles(directoryPath, ext))
                .ToArray();

            if (ablFiles.Length == 0)
            {
                Console.WriteLine($"No OpenEdge ABL files found in directory: {directoryPath}");
                return;
            }

            Console.WriteLine($"Found {ablFiles.Length} ABL files in directory: {directoryPath}");
            Console.WriteLine();

            var detector = new AblCommentDetector();
            int processedCount = 0;
            int totalExecutable = 0;
            int totalComments = 0;
            int totalMixed = 0;
            int totalEmpty = 0;
            int totalUncalled = 0;
            int totalProcedures = 0;
            int totalCalledProcedures = 0;

            // List to store detailed file analysis results
            var fileAnalysisResults = new List<FileAnalysisResult>();

            // Use project root for output paths
            string projectRoot = GetProjectRoot();

            foreach (string filePath in ablFiles)
            {
                try
                {
                    Console.WriteLine($"Processing: {Path.GetFileName(filePath)}");
                    
                    var results = detector.AnalyzeFile(filePath);

                    // Create individual output subdirectory for each file (using relative paths)
                    string fileOutputDir = Path.Combine(projectRoot, "outputs", Path.GetFileNameWithoutExtension(filePath));
                    Directory.CreateDirectory(fileOutputDir);

                    // Copy original file to output directory
                    File.Copy(filePath, Path.Combine(fileOutputDir, Path.GetFileName(filePath)), true);
                    
                    // Generate modified file in the same subdirectory
                    string modifiedFileName = $"{Path.GetFileNameWithoutExtension(filePath)}-modified{Path.GetExtension(filePath)}";
                    string modifiedOutputPath = Path.Combine(fileOutputDir, modifiedFileName);
                    detector.GenerateReport(filePath, modifiedOutputPath);

                    Console.WriteLine($"  -> Created: {fileOutputDir}/");

                    // Calculate detailed statistics for this file
                    var stats = detector.CalculateStatistics(results);
                    var procedureInfo = detector.GetProcedureInfo();
                    
                    // Collect detailed file analysis
                    var fileResult = new FileAnalysisResult
                    {
                        FileName = Path.GetFileName(filePath),
                        OriginalLines = results.Count,
                        ExecutableLines = stats.ExecutableLines + stats.MixedLines, // Mixed content is treated as executable
                        CommentLines = stats.CommentLines,
                        BlankLines = stats.EmptyLines,
                        UncalledProcedureLines = stats.UncalledProcedures,
                        CalledProcedures = procedureInfo.Values.Where(p => p.IsCalled).Select(p => p.Name).ToList(),
                        UncalledProcedures = procedureInfo.Values.Where(p => !p.IsCalled).Select(p => p.Name).ToList()
                    };
                    
                    fileAnalysisResults.Add(fileResult);

                    // Accumulate batch statistics
                    totalExecutable += stats.ExecutableLines;
                    totalComments += stats.CommentLines;
                    totalMixed += stats.MixedLines;
                    totalEmpty += stats.EmptyLines;
                    totalUncalled += stats.UncalledProcedures;

                    totalProcedures += procedureInfo.Count;
                    totalCalledProcedures += procedureInfo.Values.Count(p => p.IsCalled);

                    processedCount++;
                    detector.Reset(); // Reset for next file
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"  Error processing {Path.GetFileName(filePath)}: {ex.Message}");
                }
            }

            Console.WriteLine();
            Console.WriteLine("Analysis Results:");
            Console.WriteLine("================");
            Console.WriteLine();

            // Display detailed analysis for each file (same format as analysis-result.txt)
            foreach (var file in fileAnalysisResults.OrderBy(f => f.FileName))
            {
                Console.WriteLine($"FILE: {file.FileName}");
                Console.WriteLine("------------------");
                Console.WriteLine($"Original Lines: {file.OriginalLines}");
                Console.WriteLine($"Uncalled Procedure LOC: {file.UncalledProcedureLines}");
                Console.WriteLine($"Comments: {file.CommentLines}");
                Console.WriteLine($"Blank Lines: {file.BlankLines}");
                Console.WriteLine($"Total Unused Lines: {file.TotalUnusedLines}");
                Console.WriteLine($"Executable LOC: {file.ExecutableLines}");
                Console.WriteLine();
                Console.WriteLine($"Verification: {file.OriginalLines} = {file.UncalledProcedureLines} (uncalled) + {file.CommentLines} (comments) + {file.BlankLines} (blank) + {file.ExecutableLines} (executable)");
                Console.WriteLine();
                
                string calledProcs = file.CalledProcedures.Count > 0 ? string.Join(",", file.CalledProcedures) : "None";
                string uncalledProcs = file.UncalledProcedures.Count > 0 ? string.Join(",", file.UncalledProcedures) : "None";
                
                Console.WriteLine($"Called Procedures: {calledProcs}");
                Console.WriteLine($"Uncalled Procedures: {uncalledProcs}");
                Console.WriteLine();
                Console.WriteLine();
            }

            Console.WriteLine("Batch Processing Summary:");
            Console.WriteLine("========================");
            Console.WriteLine($"Files processed: {processedCount}/{ablFiles.Length}");
            Console.WriteLine($"Total executable lines: {totalExecutable:N0}");
            Console.WriteLine($"Total comment lines: {totalComments:N0}");
            Console.WriteLine($"Total mixed content lines: {totalMixed:N0}");
            Console.WriteLine($"Total empty lines: {totalEmpty:N0}");
            Console.WriteLine($"Total uncalled procedure lines: {totalUncalled:N0}");
            Console.WriteLine($"Total procedures found: {totalProcedures}");
            Console.WriteLine($"Total called procedures: {totalCalledProcedures}");
            Console.WriteLine($"Total uncalled procedures: {totalProcedures - totalCalledProcedures}");
            Console.WriteLine();
            Console.WriteLine($"Individual subdirectories created in: {Path.Combine(GetProjectRoot(), "outputs")}");

            // Generate analysis-result.txt
            GenerateAnalysisResultFile(fileAnalysisResults);
            
            Console.WriteLine("Analysis complete!");
        }

        /// <summary>
        /// Class representing the analysis results for a single ABL file
        /// Used both for console output and generating the analysis-result.txt file
        /// </summary>
        public class FileAnalysisResult
        {
            /// <summary>The name of the analyzed file</summary>
            public string FileName { get; set; } = "";
            
            /// <summary>Total number of lines in the file</summary>
            public int OriginalLines { get; set; }
            
            /// <summary>Number of lines containing executable code (not in uncalled procedures)</summary>
            public int ExecutableLines { get; set; }
            
            /// <summary>Number of lines containing only comments</summary>
            public int CommentLines { get; set; }
            
            /// <summary>Number of empty or whitespace-only lines</summary>
            public int BlankLines { get; set; }
            
            /// <summary>Number of lines in procedures that are never called</summary>
            public int UncalledProcedureLines { get; set; }
            
            /// <summary>List of procedure names that are called at least once</summary>
            public List<string> CalledProcedures { get; set; } = new List<string>();
            
            /// <summary>List of procedure names that are never called</summary>
            public List<string> UncalledProcedures { get; set; } = new List<string>();
            
            /// <summary>
            /// Total number of lines that are not part of executed code
            /// This includes comments, blank lines, and uncalled procedures
            /// </summary>
            public int TotalUnusedLines => UncalledProcedureLines + CommentLines + BlankLines;
        }

        /// <summary>
        /// Generates the analysis-result.txt file which contains detailed statistics 
        /// about each analyzed file, including line counts and procedure information
        /// </summary>
        /// <param name="fileResults">List of file analysis results to include in the report</param>
        static void GenerateAnalysisResultFile(List<FileAnalysisResult> fileResults)
        {
            // Use the project root to ensure consistent output location
            string projectRoot = GetProjectRoot();
            var outputPath = Path.Combine(projectRoot, "outputs", "analysis-result.txt");
            
            using (var writer = new StreamWriter(outputPath))
            {
                // Write detailed information for each file
                foreach (var file in fileResults.OrderBy(f => f.FileName))
                {
                    // File header and basic line counts
                    writer.WriteLine($"FILE: {file.FileName}");
                    writer.WriteLine("------------------");
                    writer.WriteLine($"Original Lines: {file.OriginalLines}");
                    writer.WriteLine($"Uncalled Procedure LOC: {file.UncalledProcedureLines}");
                    writer.WriteLine($"Comments: {file.CommentLines}");
                    writer.WriteLine($"Blank Lines: {file.BlankLines}");
                    writer.WriteLine($"Total Unused Lines: {file.TotalUnusedLines}");
                    writer.WriteLine($"Executable LOC: {file.ExecutableLines}");
                    writer.WriteLine();
                    
                    // Verification line showing that all categories add up to the total
                    writer.WriteLine($"Verification: {file.OriginalLines} = {file.UncalledProcedureLines} (uncalled) + {file.CommentLines} (comments) + {file.BlankLines} (blank) + {file.ExecutableLines} (executable)");
                    writer.WriteLine();
                    
                    // Lists of called and uncalled procedures
                    string calledProcs = file.CalledProcedures.Count > 0 ? string.Join(",", file.CalledProcedures) : "None";
                    string uncalledProcs = file.UncalledProcedures.Count > 0 ? string.Join(",", file.UncalledProcedures) : "None";
                    
                    writer.WriteLine($"Called Procedures: {calledProcs}");
                    writer.WriteLine($"Uncalled Procedures: {uncalledProcs}");
                    writer.WriteLine();
                    writer.WriteLine();
                }
            }
            
            Console.WriteLine($"Analysis summary saved to: {outputPath}");
        }

        /// <summary>
        /// Displays a summary of the file analysis statistics to the console
        /// </summary>
        /// <param name="results">The line-by-line analysis results</param>
        /// <param name="detector">The detector instance used for analysis</param>
        static void DisplayStatistics(System.Collections.Generic.List<AblCommentDetector.LineAnalysisResult> results, AblCommentDetector detector)
        {
            // Calculate various statistics from the analysis results
            var stats = detector.CalculateStatistics(results);
            var procedureInfo = detector.GetProcedureInfo();
            
            // Display line count breakdown by category
            Console.WriteLine("Analysis Summary:");
            Console.WriteLine($"  Total lines: {results.Count}");
            Console.WriteLine($"  Executable code: {stats.ExecutableLines} lines ({stats.ExecutablePercentage:F1}%)");
            Console.WriteLine($"  Pure comments: {stats.CommentLines} lines ({stats.CommentPercentage:F1}%)");
            Console.WriteLine($"  Mixed content: {stats.MixedLines} lines ({stats.MixedPercentage:F1}%)");
            Console.WriteLine($"  Empty lines: {stats.EmptyLines} lines ({stats.EmptyPercentage:F1}%)");
            Console.WriteLine($"  Uncalled procedures: {stats.UncalledProcedures} lines ({stats.UncalledPercentage:F1}%)");
            Console.WriteLine();

            // Display procedure count breakdown
            var calledProcedures = procedureInfo.Values.Count(p => p.IsCalled);
            var uncalledProcedures = procedureInfo.Count - calledProcedures;

            Console.WriteLine("Procedure Analysis:");
            Console.WriteLine($"  Total procedures/functions defined: {procedureInfo.Count}");
            Console.WriteLine($"  Called procedures/functions: {calledProcedures}");
            Console.WriteLine($"  Uncalled procedures/functions: {uncalledProcedures}");
            Console.WriteLine();
        }

        /// <summary>
        /// Displays examples of each line category to help users understand the classification
        /// </summary>
        /// <param name="results">The line-by-line analysis results</param>
        static void DisplayExamples(System.Collections.Generic.List<AblCommentDetector.LineAnalysisResult> results)
        {
            Console.WriteLine("Example classifications:");
            Console.WriteLine("========================");
            Console.WriteLine();

            // Show examples of executable code lines (excluding uncalled procedures)
            var executableExamples = results.Where(r => r.Type == AblCommentDetector.LineType.ExecutableCode && !r.IsUncalledProcedure).Take(3);
            if (executableExamples.Any())
            {
                Console.WriteLine("Executable Code Examples:");
                foreach (var example in executableExamples)
                {
                    // Truncate long lines for better display
                    var content = example.Content.Length > 60 ? example.Content.Substring(0, 60) + "..." : example.Content;
                    Console.WriteLine($"  Line {example.LineNumber,6}: {content}");
                }
                Console.WriteLine();
            }

            // Show examples of pure comment lines
            var commentExamples = results.Where(r => r.Type == AblCommentDetector.LineType.PureComment).Take(3);
            if (commentExamples.Any())
            {
                Console.WriteLine("Pure Comment Examples:");
                foreach (var example in commentExamples)
                {
                    var content = example.Content.Length > 60 ? example.Content.Substring(0, 60) + "..." : example.Content;
                    Console.WriteLine($"  Line {example.LineNumber,6}: {content}");
                }
                Console.WriteLine();
            }

            // Show examples of mixed content (code + comments on same line)
            var mixedExamples = results.Where(r => r.Type == AblCommentDetector.LineType.MixedContent && !r.IsUncalledProcedure).Take(3);
            if (mixedExamples.Any())
            {
                Console.WriteLine("Mixed Content Examples (treated as executable):");
                foreach (var example in mixedExamples)
                {
                    var content = example.Content.Length > 60 ? example.Content.Substring(0, 60) + "..." : example.Content;
                    Console.WriteLine($"  Line {example.LineNumber,6}: {content}");
                }
                Console.WriteLine();
            }

            // Show examples of uncalled procedure lines
            var uncalledExamples = results.Where(r => r.IsUncalledProcedure).Take(3);
            if (uncalledExamples.Any())
            {
                Console.WriteLine("Uncalled Procedure Examples:");
                foreach (var example in uncalledExamples)
                {
                    var content = example.Content.Length > 60 ? example.Content.Substring(0, 60) + "..." : example.Content;
                    Console.WriteLine($"  Line {example.LineNumber,6}: {content}");
                }
                Console.WriteLine();
            }
        }
    }
} 