# OpenEdge ABL Code Categories

This document explains the different line categories used in the ABL Comment Detector analysis, with examples from real OpenEdge ABL code.

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

```abl
/* Example: Simple executable statement */
RUN "app/main.p".       /* this one must be picked up */

/* Example: Procedure call with path */
RUN path/to/keep-4.p     /* ordinary .p file */

/* Example: Assignment operations */
CREATE IntTA68.
ASSIGN
    IntTA68.SystemRq            = IntPol68.SystemRq    
    IntTA68.MethodCode          = IntPol68.MethodCode  
    IntTA68.Policy              = IntPol68.Policy      
    IntTA68.Rencnt              = IntPol68.Rencnt

/* Example: Complex expression with built-in functions */
TB-RESPonseJourney.TransactionResponseDt   = STRING( YEAR(TODAY),"9999")
                                           + STRING(MONTH(TODAY),"99") 
                                           + STRING(  DAY(TODAY),"99")
```

### Comments

Comments are lines that contain only comment text and no executable code. There are three types:

```abl
/* Header block comment: should be ignored
   RUN ignored1.p. 
   Total lines: 132, Loc: 41, Comments: 64, Empty: 27
*/

// RUN ignored2.p       -- line comment, ignored

# RUN ignored3.i        -- hash comment, ignored

/* block start
RUN shouldNot.p
still comment */

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    
  Empty Lines: 6
  Loc: 24
  Comment: 8
------------------------------------------------------------------------------*/
```

### Blank Lines

Blank lines contain no visible characters or only whitespace:

```abl
/* Examples of blank lines: */


       

/* Blank lines before and after this comment */

```

### Uncalled Procedure LOC

These are lines within procedures that are never called with a RUN statement:

```abl
/* This procedure is uncalled in the example code */
PROCEDURE PD_GenDataIntReSend2 :
/*------------------------------------------------------------------------------
  Purpose:     Never running this procedure
  Parameters:  <none>   
  Empty Lines: 1
  Loc: 12
  Comment: 7
------------------------------------------------------------------------------*/
CREATE IntTA68.
ASSIGN
    IntTA68.SystemRq            = IntPol68.SystemRq    
    IntTA68.MethodCode          = IntPol68.MethodCode  
    IntTA68.Policy              = IntPol68.Policy      
    IntTA68.Rencnt              = IntPol68.Rencnt      
    IntTA68.Endcnt              = IntPol68.Endcnt      
    IntTA68.PolicyTypeCd        = IntPol68.PolicyTypeCd
    IntTA68.RateGroup           = IntPol68.RateGroup   
    IntTA68.PlanCode            = IntPol68.PlanCode   

END PROCEDURE.

/* Example of a RUN statement in a comment (doesn't count as a call) */
/* RUN PD_GenDataIntReSend2. */
```

For a procedure to be considered "called" and not counted as unused:
1. There must be a RUN statement followed by the procedure name
2. The RUN statement must not be inside a comment or string literal

Example of a called procedure:

```abl
/* This procedure is called in the example code */
PROCEDURE PD_GenDataIntReSend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    
  Empty Lines: 6
  Loc: 24
  Comment: 8
------------------------------------------------------------------------------*/
CREATE IntTA68.
ASSIGN
    IntTA68.SystemRq            = IntPol68.SystemRq    
    IntTA68.MethodCode          = IntPol68.MethodCode  
    IntTA68.Policy              = IntPol68.Policy      
    IntTA68.Rencnt              = IntPol68.Rencnt      
    
    /* Additional assignments... */

END PROCEDURE.

/* The following line makes PD_GenDataIntReSend a "called" procedure */
RUN PD_GenDataIntReSend.
```

### Mixed Content

Although not a separate category in the statistics, mixed content lines contain both executable code and comments:

```abl
/* Example: Inline comment after code */
RUN "app/main.p".       /* this one must be picked up */

/* Example: Code after a block comment */
/* inline block: RUN skipInline.w */  RUN keep-2.i

/* Example: Another inline comment format */
RUN  keep-3.W            /* capital "W" extension  */
```

Mixed content lines are counted as executable LOC in the statistics.

## How Totals Are Calculated

The analysis results include metrics like:

```
Original Lines: 132
Uncalled Procedure LOC: 12
Comments: 64
Blank Lines: 27
Total Unused Lines: 103
Executable LOC: 29
```

These relationships apply:
```
Original Lines = Executable LOC + Comments + Blank Lines + Uncalled Procedure LOC
Total Unused Lines = Comments + Blank Lines + Uncalled Procedure LOC
```

## Importance of Procedure Call Detection

The ABL Comment Detector specifically looks for procedure calls using the `RUN` keyword and ensures that:

1. The procedure is called with a valid `RUN` statement 
2. The `RUN` statement is not inside a comment
3. The `RUN` statement is not inside a string literal

Example of proper procedure call detection:

```abl
/* This RUN statement is not in a comment, so it counts */
RUN PD_GenDataIntReSend.

/* These RUN statements are inside comments, so they don't count */
/* RUN PD_GenDataIntReSend2. */
// RUN PD_GenDataIntReSend2.

/* This is inside a block comment that contains nested comments
/*----- Nested comment ----
RUN WRS/SendReqP4Ins.P (INPUT nv_URL2, INPUT nv_node-nameHeader2, OUTPUT ResponseResult2).
*/
/* None of the above procedures would be counted as called */
```

These metrics help developers understand how much of their codebase is actively used and how much is documentation or unused code. 