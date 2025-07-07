       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF-THEN-ELSE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 X PIC 99 VALUE 12.
       01 MSG PIC X(3).
       PROCEDURE DIVISION.
           IF X > 10
               MOVE 'yes' TO MSG
           ELSE
               MOVE 'no' TO MSG
           END-IF
           DISPLAY MSG
           STOP RUN.
