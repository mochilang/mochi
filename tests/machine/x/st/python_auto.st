| math |
math := Dictionary from:{'sqrt'->[:x | x sqrt]. 'pow'->[:x :y | x raisedTo: y]. 'sin'->[:x | x sin]. 'log'->[:x | x ln]. 'pi'->3.141592653589793e0. 'e'->2.718281828459045e0}.
Transcript show: ((math at: 'sqrt') value: 16) printString; cr.
Transcript show: (math at: 'pi') printString; cr.
