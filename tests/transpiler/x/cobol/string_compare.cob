>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. MAIN.

PROCEDURE DIVISION.
    IF "a" < "b"
        DISPLAY "True"
    ELSE
        DISPLAY "False"
    END-IF
    IF "a" <= "a"
        DISPLAY "True"
    ELSE
        DISPLAY "False"
    END-IF
    IF "b" > "a"
        DISPLAY "True"
    ELSE
        DISPLAY "False"
    END-IF
    IF "b" >= "b"
        DISPLAY "True"
    ELSE
        DISPLAY "False"
    END-IF
    STOP RUN.
