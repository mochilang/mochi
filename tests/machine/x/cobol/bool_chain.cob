       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOOL_CHAIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BOOM_RES PIC 9 VALUE 0.
       01 TMP PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       COMPUTE TMP = (1 < 2) AND (2 < 3) AND (3 < 4)
       DISPLAY TMP
       PERFORM BOOM
       COMPUTE TMP = (1 < 2) AND (2 > 3) AND BOOM_RES
       DISPLAY TMP
       PERFORM BOOM
       COMPUTE TMP = (1 < 2) AND (2 < 3) AND (3 > 4) AND BOOM_RES
       DISPLAY TMP
       STOP RUN.
       
       BOOM.
           PROCEDURE DIVISION.
               DISPLAY "boom"
               COMPUTE BOOM_RES = 1
               EXIT.
