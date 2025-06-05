# OpenEdge ABL Comment Detector

A C# application for detecting and analyzing comments in OpenEdge ABL (Progress 4GL) files.

## Overview

This tool accurately identifies comments in OpenEdge ABL code, distinguishing between:
- Single-line comments (`//`)
- Multi-line comments (`/* ... */`) with proper nesting support
- Extended block comments (`/** ... **/`)

The tool handles complex ABL syntax features including:
- String literals with comment-like content
- Line continuation characters (`~`)
- Escape sequences
- Mixed lines (containing both executable code and comments)

## Features

- Sophisticated lexical analysis with state tracking
- Proper handling of nested comments
- Context-aware parsing that understands when delimiters are syntax vs. literal text
- Automatic output file generation with "-modified.w" suffix
- Support for multiple input files
- Detailed analysis reporting

## Usage

```bash
dotnet run <input-file-path>
```

This will generate:
- A modified version of your file with comments marked
- Analysis output showing comment statistics

## Rules

- Lines with both comment and executable code are treated as executable code
- Comment markers are preserved in the output for traceability
- Nested comments are properly handled with depth tracking

## Sample Output

For input:
```
PROCEDURE PD_SAVEPD1FileAttach2 : /* This is a comment */
```

Output:
```
PROCEDURE PD_SAVEPD1FileAttach2 : /*[COMMENT]*/                    /* This is a comment */
```

## Project Structure

- `AblCommentDetector.cs`: Main detection engine
- `Program.cs`: Console application entry point
- `inputs/`: Sample input files
- `outputs/`: Generated output files with analysis results 