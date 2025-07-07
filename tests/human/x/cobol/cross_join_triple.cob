       IDENTIFICATION DIVISION.
       PROGRAM-ID. CROSS-JOIN-TRIPLE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUMS OCCURS 2 TIMES PIC 9 VALUE ZEROS.
       01 LETTERS OCCURS 2 TIMES PIC X VALUE SPACE.
       01 BOOLS OCCURS 2 TIMES PIC X(5) VALUE SPACE.
       01 I PIC 9.
       01 J PIC 9.
       01 K PIC 9.
       PROCEDURE DIVISION.
           MOVE 1 TO NUMS(1)
           MOVE 2 TO NUMS(2)
           MOVE 'A' TO LETTERS(1)
           MOVE 'B' TO LETTERS(2)
           MOVE 'true'  TO BOOLS(1)
           MOVE 'false' TO BOOLS(2)
           DISPLAY '--- Cross Join of three lists ---'
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 2
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 2
                   PERFORM VARYING K FROM 1 BY 1 UNTIL K > 2
                       DISPLAY NUMS(I) WITH NO ADVANCING
                       DISPLAY ' ' WITH NO ADVANCING
                       DISPLAY LETTERS(J) WITH NO ADVANCING
                       DISPLAY ' ' WITH NO ADVANCING
                       DISPLAY BOOLS(K)
                   END-PERFORM
               END-PERFORM
           END-PERFORM
           STOP RUN.
