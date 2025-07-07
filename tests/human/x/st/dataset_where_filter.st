| people adults |
people := {
  Dictionary newFrom: {#name->'Alice'. #age->30}.
  Dictionary newFrom: {#name->'Bob'. #age->15}.
  Dictionary newFrom: {#name->'Charlie'. #age->65}.
  Dictionary newFrom: {#name->'Diana'. #age->45}
} asOrderedCollection.
adults := people select: [:p | (p at: #age) >= 18].
Transcript show: '--- Adults ---'; cr.
adults do: [:p |
  | isSenior |
  isSenior := (p at: #age) >= 60.
  Transcript
    show: (p at: #name);
    show: ' is ';
    show: ((p at: #age) printString);
    show: (isSenior ifTrue: [' (senior)'] ifFalse: ['']);
    cr
].
