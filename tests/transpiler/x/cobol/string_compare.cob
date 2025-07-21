>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. MAIN.

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
