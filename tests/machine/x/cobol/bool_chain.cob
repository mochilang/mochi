       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOOL_CHAIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BOOM_RES PIC 9 VALUE 0.
       01 TMP PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       IF (1 < 2) AND (2 < 3) AND (3 < 4)
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       PERFORM BOOM
       IF (1 < 2) AND (2 > 3) AND BOOM_RES
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       PERFORM BOOM
       IF (1 < 2) AND (2 < 3) AND (3 > 4) AND BOOM_RES
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       STOP RUN.
       
       BOOM.
           DISPLAY "boom"
           COMPUTE BOOM_RES = 1
           EXIT.
