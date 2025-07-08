       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING_COMPARE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       IF "a" < "b"
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       IF "a" <= "a"
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       IF "b" > "a"
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       IF "b" >= "b"
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       STOP RUN.
