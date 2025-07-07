       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAP-ASSIGN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 KEYS   OCCURS 2 TIMES PIC X(10).
       01 VALUES OCCURS 2 TIMES PIC 9.
       01 LEN PIC 9 VALUE 1.
       01 IDX PIC 9.
       PROCEDURE DIVISION.
           MOVE 'alice' TO KEYS(1)
           MOVE 1      TO VALUES(1)
           ADD 1 TO LEN
           MOVE 'bob'  TO KEYS(2)
           MOVE 2      TO VALUES(2)
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > LEN
               IF KEYS(IDX) = 'bob'
                   DISPLAY VALUES(IDX)
               END-IF
           END-PERFORM
           STOP RUN.
