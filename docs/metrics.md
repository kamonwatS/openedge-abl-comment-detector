# ABL Comment Detector Metrics

This document explains how the ABL Comment Detector calculates the different metrics in its analysis report.

## Metrics Overview

When analyzing an ABL (Progress 4GL) source file, the detector reports the following metrics:

| Metric | Description |
|--------|-------------|
| Original Lines | Total number of lines in the source file |
| Uncalled Procedure LOC | Lines of code within procedures that are not called |
| Comments | Lines containing only comments |
| Blank Lines | Empty lines or lines with only whitespace |
| Total Unused Lines | Sum of Uncalled Procedure LOC, Comments, and Blank Lines |
| Executable LOC | Lines containing executable code (including mixed content) |

## How Each Metric Is Calculated

### Original Lines

This is simply a count of all lines in the source file, regardless of content.

### Uncalled Procedure LOC

A procedure is considered "uncalled" if there is no `RUN` statement referencing it outside of comments or string literals. 

The detector:
1. Identifies all procedure definitions using regex patterns
2. Tracks all procedure calls with the `RUN` keyword
3. Marks procedures with no corresponding call as "uncalled"
4. Counts all lines within these uncalled procedures

#### Example:

```abl
/* This procedure is called */
PROCEDURE CalledProcedure:
    DISPLAY "This code is counted as executable".
END PROCEDURE.

/* This RUN statement is in a comment and doesn't count as a call:
   RUN UncalledProcedure.
*/

/* This procedure is uncalled */
PROCEDURE UncalledProcedure:
    /* These 3 lines (PROCEDURE, DISPLAY, END PROCEDURE) */
    DISPLAY "This code is counted as uncalled procedure LOC".
    /* are all counted as Uncalled Procedure LOC */
END PROCEDURE.
```

### Comments

Lines that contain only comments, including:
- Block comments (`/* ... */`)
- Single-line comments (`//`)
- Preprocessor directives (`#`)

A line is considered a pure comment if:
- It's entirely within a multi-line comment block
- It begins with `//` or `#` with no code before it
- It's a complete single-line block comment (`/* ... */`)

#### Example:

```abl
/* This entire line is a comment */

// This line is also a comment

/* These multiple lines
   are all counted as
   comment lines */

CODE. /* This is a mixed line with code and comment - NOT counted as a comment line */
```

### Blank Lines

Lines that:
- Are completely empty
- Contain only whitespace characters

#### Example:

```abl
/* Code with blank lines between */

DISPLAY "Hello".

DISPLAY "World".
```

The lines between comments and code are counted as blank lines.

### Total Unused Lines

This is the sum of:
- Uncalled Procedure LOC
- Comments
- Blank Lines

It represents all lines that don't contain actively used code.

### Executable LOC

Lines containing executable code, including:
- Pure code lines
- Mixed lines (containing both code and comments)
- Lines within called procedures

A line is executable if it contains any code that will be executed at runtime.

#### Example:

```abl
/* Comment line - not executable */
DISPLAY "This is executable code".  /* Mixed line - counted as executable */

RUN SomeProcedure.  // Also executable despite trailing comment

PROCEDURE SomeProcedure:
    /* These procedure lines are executable because the procedure is called */
    DISPLAY "Called procedure code is executable".
END PROCEDURE.
```

## Complete Example with Analysis

Here's a complete example showing how the metrics would be calculated:

```abl
/* Sample ABL code for metrics demonstration */

/* Called procedure */
RUN CalledProcedure.

/* Line with executable code */
DISPLAY "Hello World".

/* Uncalled procedure */
PROCEDURE UncalledProcedure:
    DISPLAY "This procedure is never called".
END PROCEDURE.

/* Called procedure */
PROCEDURE CalledProcedure:
    DISPLAY "This procedure is called above".
END PROCEDURE.

// Another comment line
```

Analysis:
- **Original Lines**: 15 (total lines in file)
- **Uncalled Procedure LOC**: 3 (the PROCEDURE, DISPLAY, and END PROCEDURE lines in UncalledProcedure)
- **Comments**: 5 (all pure comment lines)
- **Blank Lines**: 3 (empty lines between sections)
- **Total Unused Lines**: 11 (sum of uncalled procedures, comments, and blank lines)
- **Executable LOC**: 4 (RUN statement + DISPLAY + the 2 lines in CalledProcedure)

The detector confirms this calculation by verifying: 15 = 3 + 5 + 3 + 4

## Special Cases

### Mixed Content

Lines containing both executable code and comments are classified as "Mixed Content" but are counted in the Executable LOC metric, not in the Comments metric.

### Commented Procedure Calls

The detector carefully checks that procedure calls within comments are not counted:

```abl
/* RUN CommentedProcedure. */ /* This doesn't count as a procedure call */

// RUN AnotherCommentedProcedure. /* Also doesn't count */

DISPLAY "RUN NotAProcedureCall". /* Procedure calls in strings don't count either */
``` 