# OpenEdge ABL Code Categories

This document explains the different line categories used in the ABL Comment Detector analysis, with examples of each category.

## Categories Overview

| Category | Description |
|----------|-------------|
| Original Lines | Total number of lines in the file |
| Executable LOC | Lines containing executable code that is not within an uncalled procedure |
| Comments | Lines containing only comments |
| Blank Lines | Empty lines or lines with only whitespace |
| Uncalled Procedure LOC | Lines within procedures that are never called |
| Total Unused Lines | Sum of Comments, Blank Lines, and Uncalled Procedure LOC |

## Category Examples

### Executable LOC

Executable LOC are lines of code that contain statements that will be executed. These include:

```
/* Simple executable statement */
DISPLAY "Hello, World!".

/* Variable declaration and assignment */
DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
cName = "John".

/* Database operations */
FOR EACH customer WHERE customer.state = "CA":
  DISPLAY customer.name customer.address.
END.

/* Procedure calls */
RUN calculateTotal.

/* Block with executable code */
IF cTotal > 1000 THEN DO:
  cDiscount = cTotal * 0.1.
  cTotal = cTotal - cDiscount.
END.
```

### Comments

Comments are lines that contain only comment text and no executable code. There are three types:

```
/* This is a block comment that spans
   multiple lines and contains no code */

/** This is an extended block comment
    sometimes used for documentation **/

// This is a single-line comment

/* Even a single-line block comment counts as a comment line */

# This is a preprocessor directive, also counted as a comment
```

### Blank Lines

Blank lines contain no visible characters or only whitespace:

```

    
/* Blank lines before and after this comment */

```

### Uncalled Procedure LOC

These are lines within procedures that are never called with a RUN statement:

```
/* This procedure is not called anywhere in the code */
PROCEDURE unusedProcedure:
  DISPLAY "This code is never executed".
  calculateValue(10).
END PROCEDURE.

/* But a RUN statement inside a comment doesn't count as a call */
/* RUN unusedProcedure. */

// RUN unusedProcedure.

/* Similarly, procedure calls in string literals don't count */
DISPLAY "Example: RUN unusedProcedure.".
```

For a procedure to be considered "called" and not counted as unused:
1. There must be a RUN statement followed by the procedure name
2. The RUN statement must not be inside a comment or string literal

Example of a called procedure:

```
/* This procedure is called */
PROCEDURE usedProcedure:
  DISPLAY "This code is executed".
END PROCEDURE.

/* This line makes usedProcedure "called" */
RUN usedProcedure.
```

### Mixed Content

Although not a separate category in the statistics, mixed content lines contain both executable code and comments:

```
DISPLAY "Hello". /* This is an inline comment */

OUTPUT TO file.txt. // Write to a file
```

Mixed content lines are counted as executable LOC in the statistics.

## How Totals Are Calculated

The line categories have these relationships:

```
Original Lines = Executable LOC + Comments + Blank Lines + Uncalled Procedure LOC
Total Unused Lines = Comments + Blank Lines + Uncalled Procedure LOC
```

These metrics help developers understand how much of their codebase is actively used and how much is documentation or unused code. 