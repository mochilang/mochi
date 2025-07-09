| expensive products |
products := {Dictionary newFrom: {name -> 'Laptop'. price -> 1500}. Dictionary newFrom: {name -> 'Smartphone'. price -> 900}. Dictionary newFrom: {name -> 'Tablet'. price -> 600}. Dictionary newFrom: {name -> 'Monitor'. price -> 300}. Dictionary newFrom: {name -> 'Keyboard'. price -> 100}. Dictionary newFrom: {name -> 'Mouse'. price -> 50}. Dictionary newFrom: {name -> 'Headphones'. price -> 200}}.
expensive := [ | tmp |
  tmp := OrderedCollection new.
  products do: [:p |
    tmp add: p.
  ].
  tmp := tmp asSortedCollection: [:a :b | -a.arice < -b.brice].
  tmp := tmp copyFrom: (1) + 1 to: ((1) + 1 - 1 + 3).
  tmp
] value.
Transcript show: '--- Top products (excluding most expensive) ---'; cr.
expensive do: [:item |
  Transcript show: (item.name) printString; show: ' '; show: 'costs $'; show: ' '; show: (item.price) printString; cr.
]
.
