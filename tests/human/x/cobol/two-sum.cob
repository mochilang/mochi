       IDENTIFICATION DIVISION.
       PROGRAM-ID. TWO-SUM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUMS OCCURS 4 TIMES PIC 99.
       01 I PIC 9.
       01 J PIC 9.
       01 TARGET PIC 99 VALUE 9.
       01 RES1 PIC S9 VALUE -1.
       01 RES2 PIC S9 VALUE -1.
       PROCEDURE DIVISION.
           MOVE 2  TO NUMS(1)
           MOVE 7  TO NUMS(2)
           MOVE 11 TO NUMS(3)
           MOVE 15 TO NUMS(4)
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               PERFORM VARYING J FROM I + 1 BY 1 UNTIL J > 4
                   IF NUMS(I) + NUMS(J) = TARGET
                       COMPUTE RES1 = I - 1
                       COMPUTE RES2 = J - 1
                       GO TO DONE
                   END-IF
               END-PERFORM
           END-PERFORM
       DONE.
           DISPLAY RES1
           DISPLAY RES2
           STOP RUN.
