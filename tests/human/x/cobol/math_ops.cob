       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATH-OPS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 R1 PIC 99.
       01 R2 PIC 9V9.
       01 R3 PIC 9.
       PROCEDURE DIVISION.
           COMPUTE R1 = 6 * 7
           DISPLAY R1
           COMPUTE R2 = 7 / 2
           DISPLAY R2
           COMPUTE R3 = FUNCTION MOD(7, 2)
           DISPLAY R3
           STOP RUN.
