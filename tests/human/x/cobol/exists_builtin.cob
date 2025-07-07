       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXISTS-BUILTIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DATA-LIST OCCURS 2 TIMES PIC 9 VALUE ZEROS.
       01 IDX PIC 9.
       01 FLAG PIC X(5) VALUE 'false'.
       PROCEDURE DIVISION.
           MOVE 1 TO DATA-LIST(1)
           MOVE 2 TO DATA-LIST(2)
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 2 OR FLAG = 'true'
               IF DATA-LIST(IDX) = 1
                   MOVE 'true' TO FLAG
               END-IF
           END-PERFORM
           DISPLAY FLAG
           STOP RUN.
