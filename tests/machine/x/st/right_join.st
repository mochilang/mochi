| customers orders result |
customers := {
  Dictionary newFrom:{#id->1. #name->'Alice'}.
  Dictionary newFrom:{#id->2. #name->'Bob'}.
  Dictionary newFrom:{#id->3. #name->'Charlie'}.
  Dictionary newFrom:{#id->4. #name->'Diana'}
} asOrderedCollection.
orders := {
  Dictionary newFrom:{#id->100. #customerId->1. #total->250}.
  Dictionary newFrom:{#id->101. #customerId->2. #total->125}.
  Dictionary newFrom:{#id->102. #customerId->1. #total->300}
} asOrderedCollection.

result := OrderedCollection new.
customers do: [:c |
  o := orders detect: [:ord | (ord at:#customerId) = (c at:#id)] ifNone:[nil].
  result add:(Dictionary newFrom:{#customerName->(c at:#name). #order->o})
].

Transcript show:'--- Right Join using syntax ---'; cr.
result do: [:e |
  (e at:#order) ifNotNil:[
    Transcript show:'Customer '; show:(e at:#customerName); show:' has order '; show:(((e at:#order) at:#id) printString); show:' - $'; show:(((e at:#order) at:#total) printString); cr
  ] ifNil:[
    Transcript show:'Customer '; show:(e at:#customerName); show:' has no orders'; cr
  ]
].
