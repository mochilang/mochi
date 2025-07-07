       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATASET-SORT-TAKE-LIMIT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NAMES OCCURS 7 TIMES PIC X(12).
       01 PRICES OCCURS 7 TIMES PIC 9(4).
       01 I PIC 9.
       01 J PIC 9.
       01 TEMP-N PIC X(12).
       01 TEMP-P PIC 9(4).
       PROCEDURE DIVISION.
           MOVE 'Laptop'      TO NAMES(1)
           MOVE 1500          TO PRICES(1)
           MOVE 'Smartphone'  TO NAMES(2)
           MOVE 900           TO PRICES(2)
           MOVE 'Tablet'      TO NAMES(3)
           MOVE 600           TO PRICES(3)
           MOVE 'Monitor'     TO NAMES(4)
           MOVE 300           TO PRICES(4)
           MOVE 'Keyboard'    TO NAMES(5)
           MOVE 100           TO PRICES(5)
           MOVE 'Mouse'       TO NAMES(6)
           MOVE 50            TO PRICES(6)
           MOVE 'Headphones'  TO NAMES(7)
           MOVE 200           TO PRICES(7)
           * sort by price descending (simple bubble sort)
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7 - I
                   IF PRICES(J) < PRICES(J + 1)
                       MOVE PRICES(J)     TO TEMP-P
                       MOVE PRICES(J + 1) TO PRICES(J)
                       MOVE TEMP-P        TO PRICES(J + 1)
                       MOVE NAMES(J)      TO TEMP-N
                       MOVE NAMES(J + 1)  TO NAMES(J)
                       MOVE TEMP-N        TO NAMES(J + 1)
                   END-IF
               END-PERFORM
           END-PERFORM
           DISPLAY '--- Top products (excluding most expensive) ---'
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 4
               DISPLAY NAMES(I) ' costs $ ' PRICES(I)
           END-PERFORM
           STOP RUN.
