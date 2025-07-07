| customers orders groups stats |
customers := {
  Dictionary newFrom:{#id->1. #name->'Alice'}.
  Dictionary newFrom:{#id->2. #name->'Bob'}
} asOrderedCollection.
orders := {
  Dictionary newFrom:{#id->100. #customerId->1}.
  Dictionary newFrom:{#id->101. #customerId->1}.
  Dictionary newFrom:{#id->102. #customerId->2}
} asOrderedCollection.

"Group orders by customer name"
groups := Dictionary new.
orders do: [:o |
  | c name g |
  c := customers detect: [:x | (x at:#id) = (o at:#customerId)].
  name := c at:#name.
  g := groups at: name ifAbsentPut:[OrderedCollection new].
  g add:o
].

stats := OrderedCollection new.
groups keysAndValuesDo: [:name :grp |
  stats add: (Dictionary newFrom:{#name->name. #count->grp size})
].

Transcript show:'--- Orders per customer ---'; cr.
stats do: [:s |
  Transcript show:(s at:#name); show:' orders:'; show:((s at:#count) printString); cr
].
