| items result |
items := {
  Dictionary newFrom: {#n->1. #v->'a'}.
  Dictionary newFrom: {#n->1. #v->'b'}.
  Dictionary newFrom: {#n->2. #v->'c'}
} asOrderedCollection.
result := (items asSortedCollection: [:x :y | (x at: #n) < (y at: #n)]) collect: [:e | e at: #v].
Transcript show: result printString; cr.
