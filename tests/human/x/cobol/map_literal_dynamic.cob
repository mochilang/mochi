       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAP-LITERAL-DYNAMIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 X PIC 9 VALUE 3.
       01 Y PIC 9 VALUE 4.
       01 KEYS   OCCURS 2 TIMES PIC X.
       01 VALUES OCCURS 2 TIMES PIC 9.
       01 IDX PIC 9.
       01 VALA PIC 9.
       01 VALB PIC 9.
       PROCEDURE DIVISION.
           MOVE 'a' TO KEYS(1)
           MOVE X   TO VALUES(1)
           MOVE 'b' TO KEYS(2)
           MOVE Y   TO VALUES(2)
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 2
               IF KEYS(IDX) = 'a'
                   MOVE VALUES(IDX) TO VALA
               ELSE
                   IF KEYS(IDX) = 'b'
                       MOVE VALUES(IDX) TO VALB
                   END-IF
               END-IF
           END-PERFORM
           DISPLAY VALA WITH NO ADVANCING
           DISPLAY ' ' WITH NO ADVANCING
           DISPLAY VALB
           STOP RUN.
