       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOOL-CHAIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 RES PIC X(5).
       PROCEDURE DIVISION.
           IF 1 < 2 AND 2 < 3 AND 3 < 4
               MOVE 'true' TO RES
           ELSE
               MOVE 'false' TO RES
           END-IF
           DISPLAY RES
           MOVE 'false' TO RES
           IF 1 < 2
               IF 2 > 3
                   MOVE 'true' TO RES
               END-IF
           END-IF
           DISPLAY RES
           MOVE 'false' TO RES
           IF 1 < 2
               IF 2 < 3
                   IF 3 > 4
                       MOVE 'true' TO RES
                   END-IF
               END-IF
           END-IF
           DISPLAY RES
           STOP RUN.
