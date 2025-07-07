       IDENTIFICATION DIVISION.
       PROGRAM-ID. BINARY-PRECEDENCE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 R PIC 99.
       PROCEDURE DIVISION.
           COMPUTE R = 1 + 2 * 3
           DISPLAY R
           COMPUTE R = (1 + 2) * 3
           DISPLAY R
           COMPUTE R = 2 * 3 + 1
           DISPLAY R
           COMPUTE R = 2 * (3 + 1)
           DISPLAY R
           STOP RUN.
