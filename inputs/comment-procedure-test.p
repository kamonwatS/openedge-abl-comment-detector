/*========================================================================*/
/* Test file for procedure call detection                                 */
/*========================================================================*/

/* Regular procedure - should be detected as called */
RUN CalledProcedure.

/* Procedure call inside a block comment - should NOT be detected as called
RUN CommentedProcedure1.
*/

// Procedure call inside a single-line comment - should NOT be detected as called
// RUN CommentedProcedure2.

/* Mixed line with executable code and a procedure call inside a comment */
OUTPUT TO test.txt. /* RUN CommentedProcedure3. */

/* Another test with multiple calls on the same line */
RUN CalledProcedure2. RUN CalledProcedure3.

/* Procedures with similar names but one is commented */
RUN ActuallyCalledProcedure.
/* RUN NotActuallyCalledProcedure. */

/* Procedure call inside a string - not a real call */
DISPLAY "This is an example: RUN NotReallyCalledProcedure.".

/*------------ Procedure Definitions ------------*/

/* This procedure should be detected as called */
PROCEDURE CalledProcedure:
    DISPLAY "This procedure is called!".
END PROCEDURE.

/* This procedure should be detected as called */
PROCEDURE CalledProcedure2:
    DISPLAY "This procedure is also called!".
END PROCEDURE.

/* This procedure should be detected as called */
PROCEDURE CalledProcedure3:
    DISPLAY "This procedure is called as well!".
END PROCEDURE.

/* This procedure should be detected as called */
PROCEDURE ActuallyCalledProcedure:
    DISPLAY "This procedure is indeed called!".
END PROCEDURE.

/* These procedures should NOT be detected as called since the RUN statements are in comments */
PROCEDURE CommentedProcedure1:
    DISPLAY "This procedure is NOT called!".
END PROCEDURE.

PROCEDURE CommentedProcedure2:
    DISPLAY "This procedure is NOT called either!".
END PROCEDURE.

PROCEDURE CommentedProcedure3:
    DISPLAY "This procedure is NOT called as well!".
END PROCEDURE.

PROCEDURE NotActuallyCalledProcedure:
    DISPLAY "This procedure is NOT called despite similar name!".
END PROCEDURE.

PROCEDURE NotReallyCalledProcedure:
    DISPLAY "This procedure is NOT called, it's just in a string!".
END PROCEDURE. 