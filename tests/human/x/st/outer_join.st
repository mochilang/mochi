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
  Dictionary newFrom:{#id->102. #customerId->1. #total->300}.
  Dictionary newFrom:{#id->103. #customerId->5. #total->80}
} asOrderedCollection.

result := OrderedCollection new.
orders do: [:o |
  c := customers detect: [:cust | (cust at:#id) = (o at:#customerId)] ifNone:[nil].
  result add:(Dictionary newFrom:{#order->o. #customer->c})
].
customers do: [:c |
  (orders anySatisfy: [:o | (o at:#customerId) = (c at:#id)]) ifFalse:[
    result add:(Dictionary newFrom:{#order->nil. #customer->c})
  ]
].

Transcript show:'--- Outer Join using syntax ---'; cr.
result do: [:row |
  (row at:#order) ifNotNil:[
    (row at:#customer) ifNotNil:[
      Transcript show:'Order '; show:(((row at:#order) at:#id) printString); show:' by '; show:(((row at:#customer) at:#name)); show:' - $'; show:(((row at:#order) at:#total) printString); cr
    ] ifNil:[
      Transcript show:'Order '; show:(((row at:#order) at:#id) printString); show:' by Unknown - $'; show:(((row at:#order) at:#total) printString); cr
    ]
  ] ifNil:[
    Transcript show:'Customer '; show:(((row at:#customer) at:#name)); show:' has no orders'; cr
  ]
].
