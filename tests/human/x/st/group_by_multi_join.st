| nations suppliers partsupp filtered groups result |
nations := {
  Dictionary newFrom:{#id->1. #name->'A'}.
  Dictionary newFrom:{#id->2. #name->'B'}
} asOrderedCollection.
suppliers := {
  Dictionary newFrom:{#id->1. #nation->1}.
  Dictionary newFrom:{#id->2. #nation->2}
} asOrderedCollection.
partsupp := {
  Dictionary newFrom:{#part->100. #supplier->1. #cost->10.0. #qty->2}.
  Dictionary newFrom:{#part->100. #supplier->2. #cost->20.0. #qty->1}.
  Dictionary newFrom:{#part->200. #supplier->1. #cost->5.0. #qty->3}
} asOrderedCollection.

filtered := OrderedCollection new.
partsupp do: [:ps |
  | s n |
  s := suppliers detect: [:x | (x at:#id) = (ps at:#supplier)].
  n := nations detect: [:y | (y at:#id) = (s at:#nation)].
  (n at:#name) = 'A' ifTrue:[
    filtered add:(Dictionary newFrom:{#part->(ps at:#part). #value->((ps at:#cost) * (ps at:#qty))})
  ]
].

groups := Dictionary new.
filtered do: [:x |
  | g |
  g := groups at:(x at:#part) ifAbsentPut:[OrderedCollection new].
  g add:x
].

result := OrderedCollection new.
groups keysAndValuesDo: [:part :grp |
  | total |
  total := grp inject:0 into:[:s :v | s + (v at:#value)].
  result add:(Dictionary newFrom:{#part->part. #total->total})
].
Transcript show: result printString; cr.
