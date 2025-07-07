       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLICE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ARR OCCURS 3 TIMES PIC 9.
       01 TEXT PIC X(5) VALUE "hello".
       01 SUB PIC X(3).
       PROCEDURE DIVISION.
           MOVE 1 TO ARR(1)
           MOVE 2 TO ARR(2)
           MOVE 3 TO ARR(3)
           DISPLAY ARR(2) ' ' ARR(3)
           DISPLAY ARR(1) ' ' ARR(2)
           MOVE TEXT(2:3) TO SUB
           DISPLAY SUB
           STOP RUN.
