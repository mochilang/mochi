| customers orders stats |
customers := {
  Dictionary newFrom:{#id->1. #name->'Alice'}.
  Dictionary newFrom:{#id->2. #name->'Bob'}.
  Dictionary newFrom:{#id->3. #name->'Charlie'}
} asOrderedCollection.
orders := {
  Dictionary newFrom:{#id->100. #customerId->1}.
  Dictionary newFrom:{#id->101. #customerId->1}.
  Dictionary newFrom:{#id->102. #customerId->2}
} asOrderedCollection.

stats := OrderedCollection new.
customers do: [:c |
  | count |
  count := orders select: [:o | (o at:#customerId) = (c at:#id)] size.
  stats add:(Dictionary newFrom:{#name->(c at:#name). #count->count})
].

Transcript show:'--- Group Left Join ---'; cr.
stats do: [:s | Transcript show:(s at:#name); show:' orders:'; show:((s at:#count) printString); cr].
