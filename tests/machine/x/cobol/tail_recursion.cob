       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAIL_RECURSION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FN_SUM_REC_RES PIC 9 VALUE 0.
       01 TMP PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       PERFORM FN_SUM_REC USING 10 0
       DISPLAY FN_SUM_REC_RES
       STOP RUN.
       
       FN_SUM_REC.
           PROCEDURE DIVISION USING N ACC.
               IF N = 0
                   COMPUTE FN_SUM_REC_RES = ACC
                   EXIT.
               END-IF
               COMPUTE TMP = N - 1
               COMPUTE TMP = ACC + N
               PERFORM FN_SUM_REC USING TMP TMP
               COMPUTE FN_SUM_REC_RES = FN_SUM_REC_RES
               EXIT.
