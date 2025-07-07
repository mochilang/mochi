       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAP-INDEX.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 KEYS   OCCURS 2 TIMES PIC X(1).
       01 VALUES OCCURS 2 TIMES PIC 9.
       01 IDX PIC 9.
       PROCEDURE DIVISION.
           MOVE 'a' TO KEYS(1)
           MOVE 1   TO VALUES(1)
           MOVE 'b' TO KEYS(2)
           MOVE 2   TO VALUES(2)
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 2
               IF KEYS(IDX) = 'b'
                   DISPLAY VALUES(IDX)
               END-IF
           END-PERFORM
           STOP RUN.
