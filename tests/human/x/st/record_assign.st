| c |
c := Dictionary newFrom: {#n->0}.
c at: #n put: (c at: #n) + 1.
Transcript show: (c at: #n) printString; cr.
