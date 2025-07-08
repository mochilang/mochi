       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOOL_CHAIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BOOM_RES PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       DISPLAY (1 < 2) AND (2 < 3) AND (3 < 4)
       PERFORM BOOM
       DISPLAY (1 < 2) AND (2 > 3) AND BOOM_RES
       PERFORM BOOM
       DISPLAY (1 < 2) AND (2 < 3) AND (3 > 4) AND BOOM_RES
       STOP RUN.
       
       BOOM.
           PROCEDURE DIVISION.
               DISPLAY "boom"
               COMPUTE BOOM_RES = 1
               EXIT.
