| people adults person |
people := {Dictionary newFrom:{'name'->'Alice'. 'age'->30}. Dictionary newFrom:{'name'->'Bob'. 'age'->15}. Dictionary newFrom:{'name'->'Charlie'. 'age'->65}. Dictionary newFrom:{'name'->'Diana'. 'age'->45}}.
adults := [ | tmp |
  tmp := OrderedCollection new.
  people do: [:person |
    ((person at: 'age' >= 18)) ifTrue: [
      tmp add: Dictionary newFrom:{'name'->person at: 'name'. 'age'->person at: 'age'. 'is_senior'->(person at: 'age' >= 60)}.
    ].
  ].
  tmp
] value.
Transcript show: '--- Adults ---'; cr.
adults do: [:person |.
Transcript show: (person at: 'name') printString; show: ' '; show: 'is'; show: ' '; show: (person at: 'age') printString; show: ' '; show: ((person at: 'is_senior') ifTrue: [' (senior)'] ifFalse: ['']) printString; cr.
].
.
