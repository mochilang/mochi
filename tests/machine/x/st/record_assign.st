| inc c |
inc := [:c | c at: 'n' put: (c at: 'n' + 1). ].
c := Dictionary newFrom: {#n->0}.
inc value: c.
Transcript show: (c at: 'n') printString; cr.
