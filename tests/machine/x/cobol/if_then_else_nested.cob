       IF X > 10
           COMPUTE TMP = "big"
       ELSE
           IF X > 5
               COMPUTE TMP = "medium"
           ELSE
               COMPUTE TMP = "small"
           END-IF
       END-IF
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF_THEN_ELSE_NESTED.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 X PIC 9 VALUE 8.
       01 TMP PIC 9(9) VALUE 0.
       01 MSG PIC 9.
       PROCEDURE DIVISION.
       COMPUTE MSG = TMP
       DISPLAY MSG
       STOP RUN.

