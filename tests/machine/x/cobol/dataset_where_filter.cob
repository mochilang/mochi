       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATASET-WHERE-FILTER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NAMES OCCURS 4 TIMES PIC X(10).
       01 AGES  OCCURS 4 TIMES PIC 99.
       01 I PIC 9.
       PROCEDURE DIVISION.
           MOVE 'Alice'   TO NAMES(1)
           MOVE 30        TO AGES(1)
           MOVE 'Bob'     TO NAMES(2)
           MOVE 15        TO AGES(2)
           MOVE 'Charlie' TO NAMES(3)
           MOVE 65        TO AGES(3)
           MOVE 'Diana'   TO NAMES(4)
           MOVE 45        TO AGES(4)
           DISPLAY '--- Adults ---'
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
               IF AGES(I) >= 18
                   DISPLAY NAMES(I) ' is ' AGES(I) WITH NO ADVANCING
                   IF AGES(I) >= 60
                       DISPLAY '  (senior)'
                   ELSE
                       DISPLAY ''
                   END-IF
               END-IF
           END-PERFORM
           STOP RUN.
