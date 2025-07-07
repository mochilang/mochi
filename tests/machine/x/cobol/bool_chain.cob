       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOOL-CHAIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 OUTVAL PIC X(5).
       PROCEDURE DIVISION.
           IF 1 < 2 AND 2 < 3 AND 3 < 4
               MOVE "true" TO OUTVAL
           ELSE
               MOVE "false" TO OUTVAL
           END-IF
           DISPLAY OUTVAL
           IF 1 < 2 AND 2 > 3
               MOVE "true" TO OUTVAL
           ELSE
               MOVE "false" TO OUTVAL
           END-IF
           DISPLAY OUTVAL
           IF 1 < 2 AND 2 < 3 AND 3 > 4
               MOVE "true" TO OUTVAL
           ELSE
               MOVE "false" TO OUTVAL
           END-IF
           DISPLAY OUTVAL
           STOP RUN.
