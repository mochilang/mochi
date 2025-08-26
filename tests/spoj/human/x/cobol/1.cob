       *> Solution for SPOJ TEST - Life, the Universe, and Everything
       *> https://www.spoj.com/problems/TEST/
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIFE-UNIVERSE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 N PIC X(2).
       PROCEDURE DIVISION.
           PERFORM UNTIL N = "42"
               ACCEPT N
               IF N NOT = "42"
                   DISPLAY FUNCTION TRIM(N)
               END-IF
           END-PERFORM
           STOP RUN.
