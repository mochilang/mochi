>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. MAIN.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 I PIC 9 VALUE 0.

PROCEDURE DIVISION.
    PERFORM VARYING I FROM 1 BY 1 UNTIL I >= 4
    DISPLAY I
END-PERFORM
    STOP RUN.
