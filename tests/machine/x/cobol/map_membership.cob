       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAP_MEMBERSHIP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TMP PIC 9(9) VALUE 0.
       PROCEDURE DIVISION.
       COMPUTE TMP = ("a" = "a" OR "a" = "b")
       DISPLAY TMP
       COMPUTE TMP = ("c" = "a" OR "c" = "b")
       DISPLAY TMP
       STOP RUN.
