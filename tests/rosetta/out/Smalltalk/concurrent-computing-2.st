" Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
"
| shuffle arr i j tmp w |
shuffle := [:xs | arr := xs.
i := (((arr size)) - 1).
[(i > 0)] whileTrue: [.
j := ((now value) % ((i + 1))).
tmp := arr at: i.
arr at: i put: arr at: j.
arr at: j put: tmp.
i := (i - 1).
].
.
arr. ].
shuffle value: {'Enjoy'. 'Rosetta'. 'Code'} do: [:w |.
Transcript show: (w) printString; cr.
].
.
