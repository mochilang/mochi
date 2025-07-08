       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAIL_RECURSION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SUM_REC_RES PIC 9 VALUE 0.
       01 TMP PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       PERFORM SUM_REC USING 10 0
       DISPLAY SUM_REC_RES
       STOP RUN.
       
       SUM_REC.
           PROCEDURE DIVISION USING N ACC.
               IF N = 0
                   COMPUTE SUM_REC_RES = ACC
                   EXIT.
               END-IF
               PERFORM SUM_REC USING N - 1 ACC + N
               COMPUTE SUM_REC_RES = SUM_REC_RES
               EXIT.
