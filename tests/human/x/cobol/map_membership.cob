       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAP-MEMBERSHIP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 KEYS   OCCURS 2 TIMES PIC X.
       01 VALUES OCCURS 2 TIMES PIC 9.
       01 IDX PIC 9.
       01 FOUND PIC X(5).
       PROCEDURE DIVISION.
           MOVE 'a' TO KEYS(1)
           MOVE 1   TO VALUES(1)
           MOVE 'b' TO KEYS(2)
           MOVE 2   TO VALUES(2)
           MOVE 'false' TO FOUND
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 2
               IF KEYS(IDX) = 'a'
                   MOVE 'true' TO FOUND
               END-IF
           END-PERFORM
           DISPLAY FOUND
           MOVE 'false' TO FOUND
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 2
               IF KEYS(IDX) = 'c'
                   MOVE 'true' TO FOUND
               END-IF
           END-PERFORM
           DISPLAY FOUND
           STOP RUN.
