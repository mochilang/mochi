       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING-CONCAT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 S1 PIC X(5) VALUE "hello".
       01 S2 PIC X(5) VALUE "world".
       01 RESULT PIC X(11).
       PROCEDURE DIVISION.
           STRING S1 DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  S2 DELIMITED BY SIZE
              INTO RESULT
           DISPLAY RESULT
           STOP RUN.
