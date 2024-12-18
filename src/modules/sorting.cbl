       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADVANCED-SORTING.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS DISPLAY-DEVICE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SORT-METRICS.
          05 COMPARISONS    PIC 9(10) VALUE 0.
          05 SWAPS         PIC 9(10) VALUE 0.
          05 START-TIME    PIC 9(18).
          05 END-TIME      PIC 9(18).
          05 ELAPSED-TIME  PIC 9(18).

       01 HEAP-VARS.
          05 HEAP-SIZE     PIC 9(4).
          05 PARENT-IDX    PIC 9(4).
          05 LEFT-CHILD    PIC 9(4).
          05 RIGHT-CHILD   PIC 9(4).
          05 LARGEST      PIC 9(4).

       01 QUICK-VARS.
          05 PIVOT        PIC S9(9)V99.
          05 PARTITION-IDX PIC 9(4).
          05 STACK-SIZE    PIC 9(4) VALUE 1000.
          05 STACK-TOP     PIC 9(4) VALUE 0.
          05 RANGE-STACK   OCCURS 1000 TIMES.
             10 LOW-BOUND  PIC 9(4).
             10 HIGH-BOUND PIC 9(4).

       01 MERGE-VARS.
          05 TEMP-ARRAY    OCCURS 1000 TIMES PIC S9(9)V99.
          05 MID-POINT     PIC 9(4).
          05 LEFT-POS      PIC 9(4).
          05 RIGHT-POS     PIC 9(4).
          05 MERGE-POS     PIC 9(4).

       01 COUNTERS.
          05 I            PIC 9(4).
          05 J            PIC 9(4).
          05 K            PIC 9(4).
          05 TEMP-VAL     PIC S9(9)V99.

       LINKAGE SECTION.
       01 SORT-PARAMS.
          05 ALGORITHM-TYPE PIC 9(1).
             88 USE-QUICKSORT  VALUE 1.
             88 USE-HEAPSORT   VALUE 2.
             88 USE-MERGESORT  VALUE 3.
             88 USE-TIMSORT    VALUE 4.
          05 ARRAY-SIZE     PIC 9(4).
          05 INPUT-ARRAY    OCCURS 1000 TIMES PIC S9(9)V99.
       01 OUTPUT-METRICS.
          05 COMP-COUNT     PIC 9(10).
          05 SWAP-COUNT     PIC 9(10).
          05 SORT-TIME      PIC 9(18).

       PROCEDURE DIVISION USING SORT-PARAMS OUTPUT-METRICS.
       MAIN-PROCEDURE.
           MOVE FUNCTION CURRENT-DATE TO START-TIME
           EVALUATE TRUE
               WHEN USE-QUICKSORT
                   PERFORM QUICKSORT-ENTRY
               WHEN USE-HEAPSORT
                   PERFORM HEAPSORT
               WHEN USE-MERGESORT
                   PERFORM MERGESORT-ENTRY
               WHEN USE-TIMSORT
                   PERFORM TIMSORT
           END-EVALUATE
           MOVE FUNCTION CURRENT-DATE TO END-TIME
           COMPUTE ELAPSED-TIME = END-TIME - START-TIME
           PERFORM OUTPUT-RESULTS
           GOBACK.

       QUICKSORT-ENTRY.
           MOVE 1 TO LOW-BOUND(1)
           MOVE ARRAY-SIZE TO HIGH-BOUND(1)
           MOVE 1 TO STACK-TOP
           PERFORM QUICKSORT UNTIL STACK-TOP = 0.

       QUICKSORT.
           MOVE LOW-BOUND(STACK-TOP) TO I
           MOVE HIGH-BOUND(STACK-TOP) TO J
           SUBTRACT 1 FROM STACK-TOP
           IF I < J
               PERFORM PARTITION
               IF PARTITION-IDX - I > J - PARTITION-IDX
                   PERFORM PUSH-LEFT-PARTITION
                   PERFORM PUSH-RIGHT-PARTITION
               ELSE
                   PERFORM PUSH-RIGHT-PARTITION
                   PERFORM PUSH-LEFT-PARTITION
               END-IF
           END-IF.

       PARTITION.
           MOVE INPUT-ARRAY(J) TO PIVOT
           MOVE I TO K
           PERFORM VARYING J FROM I BY 1 UNTIL J > HIGH-BOUND(STACK-TOP)
               ADD 1 TO COMPARISONS
               IF INPUT-ARRAY(J) <= PIVOT
                   PERFORM SWAP-ELEMENTS
                   ADD 1 TO K
               END-IF
           END-PERFORM
           MOVE K TO PARTITION-IDX.

       HEAPSORT.
           PERFORM BUILD-MAX-HEAP
           PERFORM VARYING I FROM ARRAY-SIZE BY -1 UNTIL I <= 1
               PERFORM SWAP-WITH-ROOT
               SUBTRACT 1 FROM HEAP-SIZE
               PERFORM HEAPIFY
           END-PERFORM.

       BUILD-MAX-HEAP.
           MOVE ARRAY-SIZE TO HEAP-SIZE
           PERFORM VARYING I FROM HEAP-SIZE / 2 BY -1 
           UNTIL I <= 1
               PERFORM HEAPIFY
           END-PERFORM.

       HEAPIFY.
           COMPUTE LEFT-CHILD = 2 * I
           COMPUTE RIGHT-CHILD = 2 * I + 1
           MOVE I TO LARGEST
           IF LEFT-CHILD <= HEAP-SIZE AND 
              INPUT-ARRAY(LEFT-CHILD) > INPUT-ARRAY(LARGEST)
               MOVE LEFT-CHILD TO LARGEST
           END-IF
           IF RIGHT-CHILD <= HEAP-SIZE AND 
              INPUT-ARRAY(RIGHT-CHILD) > INPUT-ARRAY(LARGEST)
               MOVE RIGHT-CHILD TO LARGEST
           END-IF
           IF LARGEST NOT = I
               PERFORM SWAP-ELEMENTS
               MOVE LARGEST TO I
               PERFORM HEAPIFY
           END-IF.

       MERGESORT-ENTRY.
           MOVE 1 TO I
           MOVE ARRAY-SIZE TO J
           PERFORM MERGESORT.

       MERGESORT.
           IF I < J
               COMPUTE MID-POINT = (I + J) / 2
               MOVE I TO LEFT-POS
               MOVE MID-POINT TO RIGHT-POS
               PERFORM MERGESORT
               ADD 1 TO MID-POINT
               MOVE MID-POINT TO LEFT-POS
               MOVE J TO RIGHT-POS
               PERFORM MERGESORT
               PERFORM MERGE-ARRAYS
           END-IF.

       MERGE-ARRAYS.
           MOVE I TO LEFT-POS
           COMPUTE MID-POINT = (I + J) / 2
           MOVE MID-POINT TO RIGHT-POS
           MOVE I TO MERGE-POS
           PERFORM UNTIL LEFT-POS > MID-POINT OR 
                         RIGHT-POS > J
               ADD 1 TO COMPARISONS
               IF INPUT-ARRAY(LEFT-POS) <= INPUT-ARRAY(RIGHT-POS)
                   MOVE INPUT-ARRAY(LEFT-POS) TO 
                        TEMP-ARRAY(MERGE-POS)
                   ADD 1 TO LEFT-POS
               ELSE
                   MOVE INPUT-ARRAY(RIGHT-POS) TO 
                        TEMP-ARRAY(MERGE-POS)
                   ADD 1 TO RIGHT-POS
               END-IF
               ADD 1 TO MERGE-POS
           END-PERFORM.

       TIMSORT.
           MOVE 32 TO K
           PERFORM VARYING I FROM 1 BY K UNTIL I > ARRAY-SIZE
               COMPUTE J = FUNCTION MIN(I + K - 1, ARRAY-SIZE)
               PERFORM INSERTION-SORT
           END-PERFORM
           PERFORM MERGE-RUNS.

       SWAP-ELEMENTS.
           MOVE INPUT-ARRAY(I) TO TEMP-VAL
           MOVE INPUT-ARRAY(J) TO INPUT-ARRAY(I)
           MOVE TEMP-VAL TO INPUT-ARRAY(J)
           ADD 1 TO SWAPS.

       PUSH-LEFT-PARTITION.
           ADD 1 TO STACK-TOP
           MOVE I TO LOW-BOUND(STACK-TOP)
           COMPUTE HIGH-BOUND(STACK-TOP) = PARTITION-IDX - 1.

       PUSH-RIGHT-PARTITION.
           ADD 1 TO STACK-TOP
           COMPUTE LOW-BOUND(STACK-TOP) = PARTITION-IDX + 1
           MOVE J TO HIGH-BOUND(STACK-TOP).

       OUTPUT-RESULTS.
           MOVE COMPARISONS TO COMP-COUNT
           MOVE SWAPS TO SWAP-COUNT
           MOVE ELAPSED-TIME TO SORT-TIME.