| customers orders items result |
customers := {
  Dictionary newFrom:{#id->1. #name->'Alice'}.
  Dictionary newFrom:{#id->2. #name->'Bob'}
} asOrderedCollection.
orders := {
  Dictionary newFrom:{#id->100. #customerId->1}.
  Dictionary newFrom:{#id->101. #customerId->2}
} asOrderedCollection.
items := { Dictionary newFrom:{#orderId->100. #sku->'a'} } asOrderedCollection.

result := OrderedCollection new.
orders do: [:o |
  c := customers detect: [:cust | (cust at:#id) = (o at:#customerId)].
  its := items select: [:it | (it at:#orderId) = (o at:#id)].
  its isEmpty ifTrue:[ result add:(Dictionary newFrom:{#orderId->(o at:#id). #name->(c at:#name). #item->nil}) ]
    ifFalse:[ its do: [:it | result add:(Dictionary newFrom:{#orderId->(o at:#id). #name->(c at:#name). #item->it}) ]]
].
Transcript show:'--- Left Join Multi ---'; cr.
result do: [:r | Transcript show:(r at:#orderId) printString; space; show:(r at:#name); space; show:((r at:#item) ifNil:['nil'] ifNotNil:[(r at:#item) printString]); cr].
