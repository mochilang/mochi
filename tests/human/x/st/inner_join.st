| customers orders result |
customers := {
  Dictionary newFrom:{#id->1. #name->'Alice'}.
  Dictionary newFrom:{#id->2. #name->'Bob'}.
  Dictionary newFrom:{#id->3. #name->'Charlie'}
} asOrderedCollection.
orders := {
  Dictionary newFrom:{#id->100. #customerId->1. #total->250}.
  Dictionary newFrom:{#id->101. #customerId->2. #total->125}.
  Dictionary newFrom:{#id->102. #customerId->1. #total->300}.
  Dictionary newFrom:{#id->103. #customerId->4. #total->80}
} asOrderedCollection.

result := OrderedCollection new.
orders do: [:o |
  c := customers detect: [:cust | (cust at:#id) = (o at:#customerId)] ifNone:[nil].
  c ifNotNil:[ result add:(Dictionary newFrom:{#orderId->(o at:#id). #customerName->(c at:#name). #total->(o at:#total)}) ]
].
Transcript show:'--- Orders with customer info ---'; cr.
result do: [:e | Transcript show:'Order '; show:((e at:#orderId) printString); show:' by '; show:(e at:#customerName); show:' - $'; show:((e at:#total) printString); cr].
