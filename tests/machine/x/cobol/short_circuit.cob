*> Generated by Mochi compiler v0.10.27 on 2025-07-17T11:45:42Z
>>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SHORT-CIRCUIT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FN-BOOM-RES PIC 9 VALUE 0.
       01 TMP PIC S9(9) VALUE 0.
       01 TMP-STR PIC Z(18).
       PROCEDURE DIVISION.
       PERFORM FN_BOOM USING 1 2
       IF 0 AND FN-BOOM-RES
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       PERFORM FN_BOOM USING 1 2
       IF 1 OR FN-BOOM-RES
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       STOP RUN.
       
       FN_BOOM.
           PROCEDURE DIVISION USING A B.
               DISPLAY "boom"
               COMPUTE FN-BOOM-RES = 1
               EXIT.
