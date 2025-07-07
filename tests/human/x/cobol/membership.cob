       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEMBERSHIP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUMS OCCURS 3 TIMES PIC 9.
       01 IDX PIC 9.
       01 FOUND PIC X VALUE 'F'.
       PROCEDURE DIVISION.
           MOVE 1 TO NUMS(1)
           MOVE 2 TO NUMS(2)
           MOVE 3 TO NUMS(3)
           MOVE 'F' TO FOUND
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3
               IF NUMS(IDX) = 2
                   MOVE 'T' TO FOUND
               END-IF
           END-PERFORM
           IF FOUND = 'T'
               DISPLAY 'true'
           ELSE
               DISPLAY 'false'
           END-IF
           MOVE 'F' TO FOUND
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3
               IF NUMS(IDX) = 4
                   MOVE 'T' TO FOUND
               END-IF
           END-PERFORM
           IF FOUND = 'T'
               DISPLAY 'true'
           ELSE
               DISPLAY 'false'
           END-IF
           STOP RUN.
