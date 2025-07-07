       IDENTIFICATION DIVISION.
       PROGRAM-ID. CROSS-JOIN-FILTER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUMS OCCURS 3 TIMES PIC 9 VALUE ZEROS.
       01 LETTERS OCCURS 2 TIMES PIC X.
       01 I PIC 9.
       01 J PIC 9.
       PROCEDURE DIVISION.
           MOVE 1 TO NUMS(1)
           MOVE 2 TO NUMS(2)
           MOVE 3 TO NUMS(3)
           MOVE 'A' TO LETTERS(1)
           MOVE 'B' TO LETTERS(2)
           DISPLAY '--- Even pairs ---'
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               IF FUNCTION MOD(NUMS(I), 2) = 0
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > 2
                       DISPLAY NUMS(I) WITH NO ADVANCING
                       DISPLAY ' ' WITH NO ADVANCING
                       DISPLAY LETTERS(J)
                   END-PERFORM
               END-IF
           END-PERFORM
           STOP RUN.
