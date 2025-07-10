       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOOL_CHAIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FN-BOOM-RES PIC 9 VALUE 0.
       01 TMP PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       IF (1 < 2) AND (2 < 3) AND (3 < 4)
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       PERFORM FN_BOOM
       IF (1 < 2) AND (2 > 3) AND FN_BOOM_RES
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       PERFORM FN_BOOM
       IF (1 < 2) AND (2 < 3) AND (3 > 4) AND FN_BOOM_RES
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       STOP RUN.
       
       FN_BOOM.
           DISPLAY "boom"
           COMPUTE FN_BOOM_RES = 1
           EXIT.
