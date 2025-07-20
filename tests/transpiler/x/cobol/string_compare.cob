>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. MAIN.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 TMP PIC S9(9) VALUE 0.

PROCEDURE DIVISION.
    IF "a" < "b"
        DISPLAY "true"
    ELSE
        DISPLAY "false"
    END-IF
    IF "a" <= "a"
        DISPLAY "true"
    ELSE
        DISPLAY "false"
    END-IF
    IF "b" > "a"
        DISPLAY "true"
    ELSE
        DISPLAY "false"
    END-IF
    IF "b" >= "b"
        DISPLAY "true"
    ELSE
        DISPLAY "false"
    END-IF
    STOP RUN.
