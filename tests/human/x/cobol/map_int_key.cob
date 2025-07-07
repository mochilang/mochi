       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAP-INT-KEY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 KEYS   OCCURS 2 TIMES PIC 9.
       01 VALUES OCCURS 2 TIMES PIC X.
       01 IDX PIC 9.
       PROCEDURE DIVISION.
           MOVE 1 TO KEYS(1)
           MOVE 'a' TO VALUES(1)
           MOVE 2 TO KEYS(2)
           MOVE 'b' TO VALUES(2)
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 2
               IF KEYS(IDX) = 1
                   DISPLAY VALUES(IDX)
               END-IF
           END-PERFORM
           STOP RUN.
