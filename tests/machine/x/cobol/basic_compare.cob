       IDENTIFICATION DIVISION.
       PROGRAM-ID. BASIC-COMPARE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A PIC 9.
       01 B PIC 9.
       PROCEDURE DIVISION.
           COMPUTE A = 10 - 3
           COMPUTE B = 2 + 2
           DISPLAY A
           IF A = 7
               DISPLAY "true"
           ELSE
               DISPLAY "false"
           END-IF
           IF B < 5
               DISPLAY "true"
           ELSE
               DISPLAY "false"
           END-IF
           STOP RUN.
