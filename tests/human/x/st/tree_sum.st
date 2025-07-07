| sumTree t |
sumTree := [:tree |
  tree isNil ifTrue:[0] ifFalse:[ (sumTree value:(tree at:#left)) + (tree at:#value) + (sumTree value:(tree at:#right)) ]
].
t := Dictionary newFrom:{
  #left->nil.
  #value->1.
  #right->(Dictionary newFrom:{#left->nil. #value->2. #right->nil})
}.
Transcript show:(sumTree value:t) printString; cr.
