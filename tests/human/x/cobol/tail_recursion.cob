       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAIL-RECURSION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 N PIC 99 VALUE 10.
       01 ACC PIC 99 VALUE 0.
       01 I PIC 99.
       PROCEDURE DIVISION.
           MOVE N TO I
           PERFORM VARYING I FROM N BY -1 UNTIL I = 0
               ADD I TO ACC
           END-PERFORM
           DISPLAY ACC
           STOP RUN.
