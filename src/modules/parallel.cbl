       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARALLEL-OPERATIONS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS DISPLAY-DEVICE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SYNC-CONTROL.
          05 THREAD-COUNT     PIC 9(2) VALUE 4.
          05 ACTIVE-THREADS   PIC 9(2) VALUE 0.
          05 MUTEX-STATUS     PIC 9(1) VALUE 0.
             88 MUTEX-LOCKED    VALUE 1.
             88 MUTEX-UNLOCKED  VALUE 0.
          05 THREAD-STATUS    OCCURS 10 TIMES.
             10 THREAD-ID     PIC 9(4).
             10 THREAD-STATE  PIC 9(1).
                88 THREAD-IDLE  VALUE 0.
                88 THREAD-BUSY  VALUE 1.
                88 THREAD-DONE  VALUE 2.

       01 SHARED-MEMORY.
          05 DATA-BLOCK      OCCURS 1000 TIMES.
             10 DATA-VALUE   PIC S9(9)V99.
             10 DATA-STATUS  PIC 9(1).
                88 DATA-READY    VALUE 1.
                88 DATA-PENDING  VALUE 0.

       01 WORK-VARS.
          05 I               PIC 9(4).
          05 J               PIC 9(4).
          05 CHUNK-SIZE      PIC 9(4).
          05 START-IDX       PIC 9(4).
          05 END-IDX         PIC 9(4).
          05 TEMP-RESULT     PIC S9(9)V99.

       LINKAGE SECTION.
       01 INPUT-PARAMS.
          05 OPERATION-TYPE  PIC 9(1).
             88 OP-MAP       VALUE 1.
             88 OP-REDUCE    VALUE 2.
             88 OP-FILTER    VALUE 3.
          05 DATA-SIZE      PIC 9(4).
          05 INPUT-DATA     OCCURS 1000 TIMES PIC S9(9)V99.
       01 OUTPUT-DATA       OCCURS 1000 TIMES PIC S9(9)V99.

       PROCEDURE DIVISION USING INPUT-PARAMS OUTPUT-DATA.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-PARALLEL
           EVALUATE TRUE
               WHEN OP-MAP
                   PERFORM PARALLEL-MAP
               WHEN OP-REDUCE
                   PERFORM PARALLEL-REDUCE
               WHEN OP-FILTER
                   PERFORM PARALLEL-FILTER
           END-EVALUATE
           PERFORM CLEANUP-PARALLEL
           GOBACK.

       INITIALIZE-PARALLEL.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > THREAD-COUNT
               SET THREAD-IDLE(I) TO TRUE
               COMPUTE CHUNK-SIZE = DATA-SIZE / THREAD-COUNT
           END-PERFORM
           SET MUTEX-UNLOCKED TO TRUE.

       PARALLEL-MAP.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > THREAD-COUNT
               COMPUTE START-IDX = ((I - 1) * CHUNK-SIZE) + 1
               IF I = THREAD-COUNT
                   MOVE DATA-SIZE TO END-IDX
               ELSE
                   COMPUTE END-IDX = I * CHUNK-SIZE
               END-IF
               PERFORM PROCESS-CHUNK
           END-PERFORM
           PERFORM WAIT-FOR-COMPLETION.

       PARALLEL-REDUCE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > DATA-SIZE
               MOVE INPUT-DATA(I) TO DATA-BLOCK(I)
           END-PERFORM
           PERFORM PARALLEL-MAP
           MOVE 0 TO TEMP-RESULT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > DATA-SIZE
               ADD DATA-BLOCK(I) TO TEMP-RESULT
           END-PERFORM
           MOVE TEMP-RESULT TO OUTPUT-DATA(1).

       PARALLEL-FILTER.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > DATA-SIZE
               IF INPUT-DATA(I) > 0
                   PERFORM ATOMIC-ADD
               END-IF
           END-PERFORM.

       PROCESS-CHUNK.
           PERFORM VARYING J FROM START-IDX BY 1 UNTIL J > END-IDX
               COMPUTE OUTPUT-DATA(J) = 
                   FUNCTION SQRT(INPUT-DATA(J) * INPUT-DATA(J))
               SET DATA-READY(J) TO TRUE
           END-PERFORM.

       ATOMIC-ADD.
           PERFORM UNTIL MUTEX-UNLOCKED
               CONTINUE
           END-PERFORM
           SET MUTEX-LOCKED TO TRUE
           COMPUTE TEMP-RESULT = TEMP-RESULT + INPUT-DATA(I)
           SET MUTEX-UNLOCKED TO TRUE.

       WAIT-FOR-COMPLETION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > DATA-SIZE
               PERFORM UNTIL DATA-READY(I)
                   CONTINUE
               END-PERFORM
           END-PERFORM.

       CLEANUP-PARALLEL.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > THREAD-COUNT
               SET THREAD-IDLE(I) TO TRUE
           END-PERFORM
           SET MUTEX-UNLOCKED TO TRUE.