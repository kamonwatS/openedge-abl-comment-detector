# OpenEdge ABL Comment Detector

A tool for detecting and analyzing comments in OpenEdge ABL (Progress 4GL) files.

## Overview

This tool identifies comments in OpenEdge ABL code, distinguishing between:
- Single-line comments (`//`)
- Multi-line comments (`/* ... */`) with proper nesting support
- Extended block comments (`/** ... **/`)

It also handles complex ABL syntax including string literals, line continuation characters, escape sequences, and mixed lines.

## Project Structure

```
.
├── run-detector.sh          # Main script for running the detector
├── setup-outputs.sh         # Script for output directory setup
├── src/
│   ├── CSharp/              # C# implementation
│   └── Shell/               # Utility scripts
├── inputs/                  # Sample ABL code files
├── outputs/                 # Generated output files
├── docs/                    # Documentation files
│   └── metrics.md           # Detailed metrics explanation
└── tests/                   # Reference test data
```

## Setup & Execution

### First-Time Setup

Run this once to set up the output directory structure:

```bash
./setup-outputs.sh
```

This creates a symbolic link between `src/CSharp/outputs` and the root-level `outputs` directory, ensuring consistent output storage regardless of where the application is run from.

### Running the Detector

```bash
# Process a single file
./run-detector.sh inputs/sample-code.p

# Process all files in a directory
./run-detector.sh inputs

# Process and verify against reference data
./run-detector.sh --verify inputs/WRSBQ7072.W
```

Direct C# execution is also possible:

```bash
dotnet run --project src/CSharp/AblCommentDetector.csproj -- <input-file-path>
```

## Features

- Accurate comment detection with proper nesting support
- Handles executable code mixed with comments
- Detailed analysis reporting with metrics (see [metrics documentation](docs/metrics.md))
- Procedure call detection (identifies uncalled procedures)
- Intelligent handling of commented code (RUN statements in comments are not treated as actual calls)
- Portable path handling with no absolute paths

## Output

The tool generates:
- Modified versions of input files with comments marked
- Statistical analysis (lines of code, comment percentage, etc.)
- Procedure usage information
- All outputs are stored in the `outputs` directory

## Sample Output

For input:
```
PROCEDURE PD_GenQMail : /* Send email */
```

Output:
```
PROCEDURE PD_GenQMail : /*[COMMENT]*/            /* Send email */
```

## Development Notes

- The `.cursorrc` file ensures consistent execution from the project root
- All output paths are resolved relative to the project root
- Verification compares analysis results with reference data in `tests/compare-test-data.txt`
- The symbolic link mechanism avoids absolute paths while maintaining consistent output locations 