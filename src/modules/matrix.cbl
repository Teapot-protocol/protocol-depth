       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATRIX-OPERATIONS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS DISPLAY-DEVICE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TEMP-STORAGE.
          05 MINOR-MATRIX.
             10 ROW OCCURS 2 TIMES.
                15 COL OCCURS 2 TIMES PIC S9(4)V99.
          05 COFACTOR    PIC S9(8)V99.
          05 I           PIC 9(2).
          05 J           PIC 9(2).
          05 K           PIC 9(2).
          05 L           PIC 9(2).
          05 ROW-SKIP    PIC 9(2).
          05 COL-SKIP    PIC 9(2).

       LINKAGE SECTION.
       01 INPUT-MATRIX.
          05 ROW OCCURS 3 TIMES.
             10 COL OCCURS 3 TIMES PIC S9(4)V99.
       01 OUTPUT-MATRIX.
          05 ROW OCCURS 3 TIMES.
             10 COL OCCURS 3 TIMES PIC S9(4)V99.

       PROCEDURE DIVISION USING INPUT-MATRIX OUTPUT-MATRIX.
       MAIN-PROCEDURE.
           PERFORM CALCULATE-ADJOINT
           GOBACK.

       CALCULATE-ADJOINT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
                   PERFORM GET-MINOR-MATRIX
                   PERFORM CALCULATE-MINOR-DETERMINANT
                   COMPUTE COFACTOR = FUNCTION MOD(I + J, 2)
                   IF COFACTOR = 1
                       COMPUTE COFACTOR = -1
                   ELSE
                       COMPUTE COFACTOR = 1
                   END-IF
                   COMPUTE OUTPUT-MATRIX(J,I) = 
                       COFACTOR * MINOR-MATRIX(1,1) * 
                       MINOR-MATRIX(2,2) -
                       COFACTOR * MINOR-MATRIX(1,2) * 
                       MINOR-MATRIX(2,1)
               END-PERFORM
           END-PERFORM.

       GET-MINOR-MATRIX.
           MOVE 1 TO K
           PERFORM VARYING ROW-SKIP FROM 1 BY 1 UNTIL ROW-SKIP > 3
               IF ROW-SKIP NOT = I
                   MOVE 1 TO L
                   PERFORM VARYING COL-SKIP FROM 1 BY 1 
                   UNTIL COL-SKIP > 3
                       IF COL-SKIP NOT = J
                           MOVE INPUT-MATRIX(ROW-SKIP,COL-SKIP) 
                           TO MINOR-MATRIX(K,L)
                           ADD 1 TO L
                       END-IF
                   END-PERFORM
                   ADD 1 TO K
               END-IF
           END-PERFORM.

       CALCULATE-MINOR-DETERMINANT.
           COMPUTE COFACTOR = 
               MINOR-MATRIX(1,1) * MINOR-MATRIX(2,2) -
               MINOR-MATRIX(1,2) * MINOR-MATRIX(2,1).