| customers orders result |
customers := {
  Dictionary newFrom:{#id->1. #name->'Alice'}.
  Dictionary newFrom:{#id->2. #name->'Bob'}
} asOrderedCollection.
orders := {
  Dictionary newFrom:{#id->100. #customerId->1. #total->250}.
  Dictionary newFrom:{#id->101. #customerId->3. #total->80}
} asOrderedCollection.

result := OrderedCollection new.
orders do: [:o |
  c := customers detect: [:cust | (cust at:#id) = (o at:#customerId)] ifNone:[nil].
  result add:(Dictionary newFrom:{#orderId->(o at:#id). #customer->c. #total->(o at:#total)})
].
Transcript show:'--- Left Join ---'; cr.
result do: [:e |
  Transcript
    show:'Order '; show:((e at:#orderId) printString); show:' customer ';
    show:((e at:#customer) ifNil:['nil'] ifNotNil:[(e at:#customer) at:#name]);
    show:' total '; show:((e at:#total) printString); cr
].
