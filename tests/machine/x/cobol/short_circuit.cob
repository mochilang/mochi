       IDENTIFICATION DIVISION.
       PROGRAM-ID. SHORT_CIRCUIT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FN-BOOM-RES PIC 9 VALUE 0.
       01 TMP PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       PERFORM FN_BOOM USING 1 2
       IF 0 AND FN_BOOM_RES
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       PERFORM FN_BOOM USING 1 2
       IF 1 OR FN_BOOM_RES
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       STOP RUN.
       
       FN_BOOM.
           PROCEDURE DIVISION USING A B.
               DISPLAY "boom"
               COMPUTE FN_BOOM_RES = 1
               EXIT.
