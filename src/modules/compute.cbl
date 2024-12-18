       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPLEX-COMPUTATIONS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS DISPLAY-DEVICE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PI              PIC 9(3)V9(14) VALUE 3.14159265358979.
       01 E               PIC 9(3)V9(14) VALUE 2.71828182845904.
       
       01 TEMP-VARS.
          05 TEMP-SIN     PIC S9(3)V9(14).
          05 TEMP-COS     PIC S9(3)V9(14).
          05 TEMP-RESULT  PIC S9(8)V9(14).
          05 FACTORIAL    PIC 9(10) VALUE 0.
          05 POWER        PIC S9(5)V9(14).
          05 TERM         PIC S9(3)V9(14).
          05 N            PIC 9(4).
          05 I            PIC 9(4).
          05 X            PIC S9(4)V9(14).

       LINKAGE SECTION.
       01 INPUT-VALUE     PIC S9(4)V99.
       01 OPERATION       PIC 9.
          88 CALC-FFT     VALUE 1.
          88 CALC-STATS   VALUE 2.
          88 CALC-SERIES  VALUE 3.
       01 RESULT-VALUE    PIC S9(8)V99.

       PROCEDURE DIVISION USING INPUT-VALUE OPERATION RESULT-VALUE.
       MAIN-PROCEDURE.
           EVALUATE TRUE
               WHEN CALC-FFT
                   PERFORM FFT-COMPUTATION
               WHEN CALC-STATS
                   PERFORM STATISTICAL-ANALYSIS
               WHEN CALC-SERIES
                   PERFORM TAYLOR-SERIES
           END-EVALUATE
           GOBACK.

       FFT-COMPUTATION.
           MOVE INPUT-VALUE TO X
           COMPUTE TEMP-SIN = 
               FUNCTION SIN(2 * PI * X)
           COMPUTE TEMP-COS = 
               FUNCTION COS(2 * PI * X)
           COMPUTE RESULT-VALUE = 
               FUNCTION SQRT(TEMP-SIN * TEMP-SIN + 
                           TEMP-COS * TEMP-COS).

       STATISTICAL-ANALYSIS.
           COMPUTE RESULT-VALUE = 
               FUNCTION RANDOM * INPUT-VALUE
           COMPUTE RESULT-VALUE = 
               RESULT-VALUE + 
               FUNCTION SQRT(FUNCTION ABS(INPUT-VALUE)).

       TAYLOR-SERIES.
           MOVE 0 TO TEMP-RESULT
           MOVE INPUT-VALUE TO X
           PERFORM VARYING N FROM 0 BY 1 UNTIL N > 10
               PERFORM CALCULATE-FACTORIAL
               COMPUTE POWER = 1
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
                   COMPUTE POWER = POWER * X
               END-PERFORM
               COMPUTE TERM = POWER / FACTORIAL
               COMPUTE TEMP-RESULT = TEMP-RESULT + TERM
           END-PERFORM
           MOVE TEMP-RESULT TO RESULT-VALUE.

       CALCULATE-FACTORIAL.
           IF N = 0
               MOVE 1 TO FACTORIAL
           ELSE
               MOVE 1 TO FACTORIAL
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
                   MULTIPLY I BY FACTORIAL
               END-PERFORM
           END-IF.