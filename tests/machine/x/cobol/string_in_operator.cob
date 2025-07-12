       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING-IN-OPERATOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 S PIC X(5) VALUE "catch".
       01 TMP PIC 9(9) VALUE 0.
       PROCEDURE DIVISION.
       MOVE 0 TO TMP
       INSPECT S TALLYING TMP FOR ALL "cat"
       IF TMP > 0
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       MOVE 0 TO TMP
       INSPECT S TALLYING TMP FOR ALL "dog"
       IF TMP > 0
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       STOP RUN.
