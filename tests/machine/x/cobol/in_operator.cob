       IDENTIFICATION DIVISION.
       PROGRAM-ID. IN-OPERATOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 XS OCCURS 3 TIMES PIC 9.
       01 IDX PIC 9.
       01 FOUND PIC X(5).
       PROCEDURE DIVISION.
           MOVE 1 TO XS(1)
           MOVE 2 TO XS(2)
           MOVE 3 TO XS(3)
           MOVE 'false' TO FOUND
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3 OR FOUND = 'true'
               IF XS(IDX) = 2
                   MOVE 'true' TO FOUND
               END-IF
           END-PERFORM
           DISPLAY FOUND
           MOVE 'false' TO FOUND
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3 OR FOUND = 'true'
               IF XS(IDX) = 5
                   MOVE 'true' TO FOUND
               END-IF
           END-PERFORM
           IF FOUND = 'true'
               DISPLAY 'false'
           ELSE
               DISPLAY 'true'
           END-IF
           STOP RUN.
