       IDENTIFICATION DIVISION.
       PROGRAM-ID. WHILE-LOOP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 I PIC 9 VALUE 0.
       PROCEDURE DIVISION.
           PERFORM UNTIL I >= 3
               DISPLAY I
               ADD 1 TO I
           END-PERFORM
           STOP RUN.
