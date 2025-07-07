       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF-THEN-ELSE-NESTED.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 X PIC 99 VALUE 8.
       01 MSG PIC X(6).
       PROCEDURE DIVISION.
           IF X > 10
               MOVE 'big' TO MSG
           ELSE
               IF X > 5
                   MOVE 'medium' TO MSG
               ELSE
                   MOVE 'small' TO MSG
               END-IF
           END-IF
           DISPLAY MSG
           STOP RUN.
