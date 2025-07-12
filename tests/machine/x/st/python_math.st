| math r area root sin45 log_e |
math := Dictionary from:{'sqrt'->[:x | x sqrt]. 'pow'->[:x :y | x raisedTo: y]. 'sin'->[:x | x sin]. 'log'->[:x | x ln]. 'pi'->3.141592653589793e0. 'e'->2.718281828459045e0}.
r := 3.
area := ((math at: 'pi') * ((math at: 'pow') value: r value: 2)).
root := (math at: 'sqrt') value: 49.
sin45 := (math at: 'sin') value: (((math at: 'pi') / 4)).
log_e := (math at: 'log') value: (math at: 'e').
Transcript show: 'Circle area with r ='; show: ' '; show: (r) printString; show: ' '; show: '=>'; show: ' '; show: (area) printString; cr.
Transcript show: 'Square root of 49:'; show: ' '; show: (root) printString; cr.
Transcript show: 'sin(π/4):'; show: ' '; show: (sin45) printString; cr.
Transcript show: 'log(e):'; show: ' '; show: (log_e) printString; cr.
