| customers orders result entry |
customers := {Dictionary newFrom:{'id'->1. 'name'->'Alice'}. Dictionary newFrom:{'id'->2. 'name'->'Bob'}. Dictionary newFrom:{'id'->3. 'name'->'Charlie'}. Dictionary newFrom:{'id'->4. 'name'->'Diana'}}.
orders := {Dictionary newFrom:{'id'->100. 'customerId'->1. 'total'->250}. Dictionary newFrom:{'id'->101. 'customerId'->2. 'total'->125}. Dictionary newFrom:{'id'->102. 'customerId'->1. 'total'->300}}.
result := [ | tmp |
  tmp := OrderedCollection new.
  orders do: [:o |
    | c |
    c := customers detect: [:c | ((o.customerId = c.id)) ] ifAbsent:[nil].
    tmp add: Dictionary newFrom:{'customerName'->c.name. 'order'->o}.
  ].
  tmp
] value.
Transcript show: '--- Right Join using syntax ---'; cr.
result do: [:entry |.
(entry at: 'order') ifTrue: [
Transcript show: 'Customer'; show: ' '; show: (entry at: 'customerName') printString; show: ' '; show: 'has order'; show: ' '; show: (entry at: 'order' at: 'id') printString; show: ' '; show: '- $'; show: ' '; show: (entry at: 'order' at: 'total') printString; cr.
] ifFalse: [
Transcript show: 'Customer'; show: ' '; show: (entry at: 'customerName') printString; show: ' '; show: 'has no orders'; cr.
].
].
.
