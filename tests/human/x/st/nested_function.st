| outer |
outer := [:x | | inner | inner := [:y | x + y ]. inner value: 5 ].
Transcript show: (outer value: 3) printString; cr.
