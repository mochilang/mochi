| testpkg |
testpkg := Dictionary from:{'Add'->[:a :b | a + b]. 'Pi'->3.14e0. 'Answer'->42}.
Transcript show: ((testpkg at: 'Add') value: 2 value: 3) printString; cr.
Transcript show: (testpkg at: 'Pi') printString; cr.
Transcript show: (testpkg at: 'Answer') printString; cr.
