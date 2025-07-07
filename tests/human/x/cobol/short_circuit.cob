       IDENTIFICATION DIVISION.
       PROGRAM-ID. SHORT-CIRCUIT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 RESULT1 PIC X(5) VALUE 'false'.
       01 RESULT2 PIC X(5) VALUE 'true'.
       PROCEDURE DIVISION.
           * false && boom()
           IF 1 = 1
               CONTINUE
           ELSE
               PERFORM BOOM
           END-IF
           DISPLAY RESULT1
           * true || boom()
           IF 1 = 0
               PERFORM BOOM
               MOVE 'true' TO RESULT2
           END-IF
           DISPLAY RESULT2
           STOP RUN.
       BOOM.
           DISPLAY 'boom'.
           EXIT.
