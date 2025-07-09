| boom |
boom := [ | Transcript show: 'boom'; cr.
^true. ].
Transcript show: ((((1 < 2) and: [(2 < 3)]) and: [(3 < 4)])) printString; cr.
Transcript show: ((((1 < 2) and: [(2 > 3)]) and: [boom])) printString; cr.
Transcript show: (((((1 < 2) and: [(2 < 3)]) and: [(3 > 4)]) and: [boom])) printString; cr.
