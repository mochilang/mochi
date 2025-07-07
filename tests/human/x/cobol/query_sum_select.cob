       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUERY-SUM-SELECT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUMS OCCURS 3 TIMES PIC 9.
       01 I PIC 9.
       01 SUM PIC 99 VALUE 0.
       PROCEDURE DIVISION.
           MOVE 1 TO NUMS(1)
           MOVE 2 TO NUMS(2)
           MOVE 3 TO NUMS(3)
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               IF NUMS(I) > 1
                   ADD NUMS(I) TO SUM
               END-IF
           END-PERFORM
           DISPLAY SUM
           STOP RUN.
