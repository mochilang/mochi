       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING-COMPARE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 RES PIC X(5).
       PROCEDURE DIVISION.
           IF "a" < "b" THEN
               MOVE 'true' TO RES
           ELSE
               MOVE 'false' TO RES
           END-IF
           DISPLAY RES
           IF "a" <= "a" THEN
               MOVE 'true' TO RES
           ELSE
               MOVE 'false' TO RES
           END-IF
           DISPLAY RES
           IF "b" > "a" THEN
               MOVE 'true' TO RES
           ELSE
               MOVE 'false' TO RES
           END-IF
           DISPLAY RES
           IF "b" >= "b" THEN
               MOVE 'true' TO RES
           ELSE
               MOVE 'false' TO RES
           END-IF
           DISPLAY RES
           STOP RUN.
