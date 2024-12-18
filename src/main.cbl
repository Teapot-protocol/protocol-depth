       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEPTH-MAIN.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS DISPLAY-DEVICE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MATRIX-A.
          05 ROW OCCURS 3 TIMES.
             10 COL OCCURS 3 TIMES PIC S9(4)V99.
       01 MATRIX-B.
          05 ROW OCCURS 3 TIMES.
             10 COL OCCURS 3 TIMES PIC S9(4)V99.
       01 RESULT-MATRIX.
          05 ROW OCCURS 3 TIMES.
             10 COL OCCURS 3 TIMES PIC S9(4)V99.
       
       01 COUNTERS.
          05 I           PIC 9(2) VALUE 1.
          05 J           PIC 9(2) VALUE 1.
          05 K           PIC 9(2) VALUE 1.
       
       01 COMPUTATION-VARS.
          05 TEMP-SUM    PIC S9(8)V99 VALUE 0.
          05 DETERMINANT PIC S9(8)V99 VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-MATRICES
           PERFORM MATRIX-MULTIPLICATION
           PERFORM CALCULATE-DETERMINANT
           PERFORM DISPLAY-RESULTS
           STOP RUN.

       INITIALIZE-MATRICES.
           MOVE 2.00 TO MATRIX-A(1,1)
           MOVE 3.00 TO MATRIX-A(1,2)
           MOVE 1.00 TO MATRIX-A(1,3)
           MOVE 4.00 TO MATRIX-A(2,1)
           MOVE 0.00 TO MATRIX-A(2,2)
           MOVE 1.00 TO MATRIX-A(2,3)
           MOVE 2.00 TO MATRIX-A(3,1)
           MOVE 1.00 TO MATRIX-A(3,2)
           MOVE 3.00 TO MATRIX-A(3,3)

           MOVE 1.00 TO MATRIX-B(1,1)
           MOVE 2.00 TO MATRIX-B(1,2)
           MOVE 1.00 TO MATRIX-B(1,3)
           MOVE 0.00 TO MATRIX-B(2,1)
           MOVE 3.00 TO MATRIX-B(2,2)
           MOVE 2.00 TO MATRIX-B(2,3)
           MOVE 2.00 TO MATRIX-B(3,1)
           MOVE 1.00 TO MATRIX-B(3,2)
           MOVE 0.00 TO MATRIX-B(3,3).

       MATRIX-MULTIPLICATION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
                   MOVE 0 TO TEMP-SUM
                   PERFORM VARYING K FROM 1 BY 1 UNTIL K > 3
                       COMPUTE TEMP-SUM = TEMP-SUM + 
                           (MATRIX-A(I,K) * MATRIX-B(K,J))
                   END-PERFORM
                   MOVE TEMP-SUM TO RESULT-MATRIX(I,J)
               END-PERFORM
           END-PERFORM.

       CALCULATE-DETERMINANT.
           COMPUTE DETERMINANT = 
               (MATRIX-A(1,1) * MATRIX-A(2,2) * MATRIX-A(3,3)) +
               (MATRIX-A(1,2) * MATRIX-A(2,3) * MATRIX-A(3,1)) +
               (MATRIX-A(1,3) * MATRIX-A(2,1) * MATRIX-A(3,2)) -
               (MATRIX-A(1,3) * MATRIX-A(2,2) * MATRIX-A(3,1)) -
               (MATRIX-A(1,1) * MATRIX-A(2,3) * MATRIX-A(3,2)) -
               (MATRIX-A(1,2) * MATRIX-A(2,1) * MATRIX-A(3,3)).

       DISPLAY-RESULTS.
           DISPLAY "Matrix Multiplication Result:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
                   DISPLAY "Result(" I "," J "): " 
                          RESULT-MATRIX(I,J)
               END-PERFORM
           END-PERFORM
           DISPLAY "Determinant of Matrix A: " DETERMINANT.