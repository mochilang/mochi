       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIN-MAX-BUILTIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUMS OCCURS 3 TIMES PIC 9.
       01 IDX PIC 9.
       01 MINVAL PIC 9.
       01 MAXVAL PIC 9.
       PROCEDURE DIVISION.
           MOVE 3 TO NUMS(1)
           MOVE 1 TO NUMS(2)
           MOVE 4 TO NUMS(3)
           MOVE NUMS(1) TO MINVAL
           MOVE NUMS(1) TO MAXVAL
           PERFORM VARYING IDX FROM 2 BY 1 UNTIL IDX > 3
               IF NUMS(IDX) < MINVAL
                   MOVE NUMS(IDX) TO MINVAL
               END-IF
               IF NUMS(IDX) > MAXVAL
                   MOVE NUMS(IDX) TO MAXVAL
               END-IF
           END-PERFORM
           DISPLAY MINVAL
           DISPLAY MAXVAL
           STOP RUN.
