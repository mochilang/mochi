       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING_PREFIX_SLICE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PREFIX PIC X(4) VALUE "fore".
       01 S1 PIC X(6) VALUE "forest".
       01 S2 PIC X(6) VALUE "desert".
       PROCEDURE DIVISION.
       IF S1(0 + 1:FUNCTION LENGTH(PREFIX) - 0) = PREFIX
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       IF S2(0 + 1:FUNCTION LENGTH(PREFIX) - 0) = PREFIX
           DISPLAY "true"
       ELSE
           DISPLAY "false"
       END-IF
       STOP RUN.
