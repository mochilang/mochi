       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAIL_RECURSION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FN-SUM-REC-RES PIC 9 VALUE 0.
       01 TMP PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       PERFORM FN_SUM-REC USING 10 0
       DISPLAY FN_SUM-REC_RES
       STOP RUN.
       
       FN_SUM-REC.
           PROCEDURE DIVISION USING N ACC.
               IF N = 0
                   COMPUTE FN_SUM-REC_RES = ACC
                   EXIT.
               END-IF
               COMPUTE TMP = N - 1
               COMPUTE TMP = ACC + N
               PERFORM FN_SUM-REC USING TMP TMP
               COMPUTE FN_SUM-REC_RES = FN_SUM-REC_RES
               EXIT.
