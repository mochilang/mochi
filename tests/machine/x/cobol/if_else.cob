       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF_ELSE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 X PIC 9 VALUE 5.
       PROCEDURE DIVISION.
       IF X > 3
           DISPLAY "big"
       ELSE
           DISPLAY "small"
       END-IF
       STOP RUN.
