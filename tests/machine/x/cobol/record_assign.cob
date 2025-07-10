       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECORD-ASSIGN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FN-INC-RES PIC 9 VALUE 0.
       01 C.
           05 N PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       PERFORM FN_INC USING C
       FN_INC_RES
       DISPLAY N
       STOP RUN.
       
       FN_INC.
           PROCEDURE DIVISION USING C.
               COMPUTE N = N + 1
               EXIT.
