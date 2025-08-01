*> Generated by Mochi compiler v0.10.30 on 2025-07-19T00:29:29Z
>>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAIL-RECURSION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FN-SUM-REC-RES PIC 9 VALUE 0.
       01 N PIC 9 VALUE 0.
       01 ACC PIC 9 VALUE 0.
       01 TMP PIC S9(9) VALUE 0.
       01 TMP-STR PIC Z(18).
       PROCEDURE DIVISION.
       COMPUTE N = 10
       COMPUTE ACC = 0
       PERFORM FN_SUM-REC
       DISPLAY FN-SUM-REC-RES
       STOP RUN.
       
       FN_SUM-REC.
           PROCEDURE DIVISION.
               IF N = 0
                   COMPUTE FN-SUM-REC-RES = ACC
                   EXIT.
               END-IF
               COMPUTE TMP = N - 1
               COMPUTE N = TMP
               COMPUTE TMP = ACC + N
               COMPUTE ACC = TMP
               PERFORM FN_SUM-REC
               COMPUTE FN-SUM-REC-RES = FN-SUM-REC-RES
               EXIT.
