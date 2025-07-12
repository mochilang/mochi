| sum_tree t |
sum_tree := [:t | ^(t = Leaf) ifTrue: [0] ifFalse: [(t = Node value: left value: value value: right) ifTrue: [((sum_tree value: left + value) + sum_tree value: right)] ifFalse: [nil]]. ].
t := Dictionary newFrom:{'left'->Leaf. 'value'->1. 'right'->Dictionary newFrom:{'left'->Leaf. 'value'->2. 'right'->Leaf}}.
Transcript show: (sum_tree value: t) printString; cr.
