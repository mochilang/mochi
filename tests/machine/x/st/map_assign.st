| scores |
scores := Dictionary newFrom:{'alice'->1}.
scores at: 'bob' put: 2.
Transcript show: (scores at: 'bob') printString; cr.
