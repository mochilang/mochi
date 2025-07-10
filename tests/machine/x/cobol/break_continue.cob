       IDENTIFICATION DIVISION.
       PROGRAM-ID. BREAK-CONTINUE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 N PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       PERFORM VARYING N FROM 1 BY 1 UNTIL N > 9
           IF FUNCTION MOD(N, 2) = 0
               CONTINUE
           END-IF
           IF N > 7
               EXIT PERFORM
           END-IF
           DISPLAY "odd number:" N
       END-PERFORM
       STOP RUN.
