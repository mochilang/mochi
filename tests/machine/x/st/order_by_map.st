| data sorted |
data := {
  Dictionary newFrom: {#a->1. #b->2}.
  Dictionary newFrom: {#a->1. #b->1}.
  Dictionary newFrom: {#a->0. #b->5}
} asOrderedCollection.
sorted := data asSortedCollection: [:x :y |
  (x at: #a) = (y at: #a)
    ifTrue: [(x at: #b) < (y at: #b)]
    ifFalse: [(x at: #a) < (y at: #a)]
].
Transcript show: sorted printString; cr.
