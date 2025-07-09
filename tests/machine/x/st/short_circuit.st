boom := [:a :b | Transcript show: ('boom') printString; cr.
^true. ].
Transcript show: ((false and: [boom value: 1 value: 2])) printString; cr.
Transcript show: ((true or: [boom value: 1 value: 2])) printString; cr.
