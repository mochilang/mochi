       IF X > 10
           COMPUTE TMP = "yes"
       ELSE
           COMPUTE TMP = "no"
       END-IF
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF-THEN-ELSE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 X PIC 9 VALUE 12.
       01 TMP PIC 9(9) VALUE 0.
       01 MSG PIC 9.
       PROCEDURE DIVISION.
       COMPUTE MSG = TMP
       DISPLAY MSG
       STOP RUN.
