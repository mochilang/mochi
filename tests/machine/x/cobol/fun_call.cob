*> Generated by Mochi compiler v0.10.30 on 2025-07-19T00:29:15Z
>>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FUN-CALL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FN-ADD-RES PIC 9 VALUE 0.
       01 A PIC 9 VALUE 0.
       01 B PIC 9 VALUE 0.
       01 TMP PIC S9(9) VALUE 0.
       01 TMP-STR PIC Z(18).
       PROCEDURE DIVISION.
       COMPUTE A = 2
       COMPUTE B = 3
       PERFORM FN_ADD
       DISPLAY FN-ADD-RES
       STOP RUN.
       
       FN_ADD.
           PROCEDURE DIVISION.
               COMPUTE FN-ADD-RES = A + B
               EXIT.
