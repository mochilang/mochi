>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. MAIN.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 PREFIX PIC X(4) VALUE "fore".
01 S1 PIC X(6) VALUE "forest".
01 S2 PIC X(6) VALUE "desert".

PROCEDURE DIVISION.
    IF S1(0 + 1:FUNCTION LENGTH(PREFIX) - 0) = PREFIX
        DISPLAY "True"
    ELSE
        DISPLAY "False"
    END-IF
    IF S2(0 + 1:FUNCTION LENGTH(PREFIX) - 0) = PREFIX
        DISPLAY "True"
    ELSE
        DISPLAY "False"
    END-IF
    STOP RUN.
