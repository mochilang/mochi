       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAP-IN-OPERATOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 KEYS   OCCURS 2 TIMES PIC 9.
       01 VALUES OCCURS 2 TIMES PIC X(1).
       01 IDX PIC 9.
       01 FLAG PIC X(5).
       PROCEDURE DIVISION.
           MOVE 1 TO KEYS(1)
           MOVE 'a' TO VALUES(1)
           MOVE 2 TO KEYS(2)
           MOVE 'b' TO VALUES(2)
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 2
               IF KEYS(IDX) = 1
                   MOVE 'true' TO FLAG
               END-IF
           END-PERFORM
           IF FLAG = 'true'
               DISPLAY 'true'
           ELSE
               DISPLAY 'false'
           END-IF
           MOVE 'false' TO FLAG
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 2
               IF KEYS(IDX) = 3
                   MOVE 'true' TO FLAG
               END-IF
           END-PERFORM
           IF FLAG = 'true'
               DISPLAY 'true'
           ELSE
               DISPLAY 'false'
           END-IF
           STOP RUN.
