       IDENTIFICATION DIVISION.
       PROGRAM-ID. GROUP-BY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NAMES OCCURS 6 TIMES PIC X(7).
       01 AGES  OCCURS 6 TIMES PIC 99.
       01 CITIES OCCURS 6 TIMES PIC X(10).
       01 I PIC 9.
       01 PARIS-COUNT PIC 9 VALUE 0.
       01 PARIS-SUM   PIC 99 VALUE 0.
       01 HANOI-COUNT PIC 9 VALUE 0.
       01 HANOI-SUM   PIC 99 VALUE 0.
       01 PARIS-AVG  PIC 99V9(15).
       01 HANOI-AVG  PIC 99V9(15).
       PROCEDURE DIVISION.
           MOVE 'Alice'   TO NAMES(1)
           MOVE 30        TO AGES(1)
           MOVE 'Paris'   TO CITIES(1)
           MOVE 'Bob'     TO NAMES(2)
           MOVE 15        TO AGES(2)
           MOVE 'Hanoi'   TO CITIES(2)
           MOVE 'Charlie' TO NAMES(3)
           MOVE 65        TO AGES(3)
           MOVE 'Paris'   TO CITIES(3)
           MOVE 'Diana'   TO NAMES(4)
           MOVE 45        TO AGES(4)
           MOVE 'Hanoi'   TO CITIES(4)
           MOVE 'Eve'     TO NAMES(5)
           MOVE 70        TO AGES(5)
           MOVE 'Paris'   TO CITIES(5)
           MOVE 'Frank'   TO NAMES(6)
           MOVE 22        TO AGES(6)
           MOVE 'Hanoi'   TO CITIES(6)
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
               EVALUATE CITIES(I)
                   WHEN 'Paris'
                       ADD 1 TO PARIS-COUNT
                       ADD AGES(I) TO PARIS-SUM
                   WHEN 'Hanoi'
                       ADD 1 TO HANOI-COUNT
                       ADD AGES(I) TO HANOI-SUM
               END-EVALUATE
           END-PERFORM
           COMPUTE PARIS-AVG = PARIS-SUM / PARIS-COUNT
           COMPUTE HANOI-AVG = HANOI-SUM / HANOI-COUNT
           DISPLAY '--- People grouped by city ---'
           DISPLAY 'Paris : count = ' PARIS-COUNT ' , avg_age = ' PARIS-AVG
           DISPLAY 'Hanoi : count = ' HANOI-COUNT ' , avg_age = ' HANOI-AVG
           STOP RUN.
