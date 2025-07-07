       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING-IN-OPERATOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 S PIC X(5) VALUE "catch".
       01 SUB PIC X(3).
       01 IDX PIC 9.
       01 FOUND PIC X(5).
       PROCEDURE DIVISION.
           MOVE 'cat' TO SUB
           MOVE 'false' TO FOUND
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3
               IF S(IDX:3) = SUB
                   MOVE 'true' TO FOUND
                   EXIT PERFORM
               END-IF
           END-PERFORM
           DISPLAY FOUND
           MOVE 'dog' TO SUB
           MOVE 'false' TO FOUND
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3
               IF S(IDX:3) = SUB
                   MOVE 'true' TO FOUND
                   EXIT PERFORM
               END-IF
           END-PERFORM
           DISPLAY FOUND
           STOP RUN.
