| people groups stats |
people := {
  Dictionary newFrom: {#name->'Alice'. #age->30. #city->'Paris'}.
  Dictionary newFrom: {#name->'Bob'. #age->15. #city->'Hanoi'}.
  Dictionary newFrom: {#name->'Charlie'. #age->65. #city->'Paris'}.
  Dictionary newFrom: {#name->'Diana'. #age->45. #city->'Hanoi'}.
  Dictionary newFrom: {#name->'Eve'. #age->70. #city->'Paris'}.
  Dictionary newFrom: {#name->'Frank'. #age->22. #city->'Hanoi'}
} asOrderedCollection.

"Group people by city"
groups := Dictionary new.
people do: [:p |
  | g |
  g := groups at: (p at: #city) ifAbsentPut: [OrderedCollection new].
  g add: p
].

stats := OrderedCollection new.
groups keysAndValuesDo: [:city :grp |
  | count avg |
  count := grp size.
  avg := (grp inject: 0 into: [:s :x | s + (x at: #age)]) / count.
  stats add: (Dictionary newFrom: {#city->city. #count->count. #avg_age->avg})
].

Transcript show: '--- People grouped by city ---'; cr.
stats do: [:s |
  Transcript
    show: (s at: #city);
    show: ': count =';
    show: ((s at: #count) printString);
    show: ', avg_age =';
    show: ((s at: #avg_age) printString);
    cr
].
