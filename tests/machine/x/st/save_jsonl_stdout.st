| people |
people := {
  Dictionary newFrom:{#name->'Alice'. #age->30}.
  Dictionary newFrom:{#name->'Bob'. #age->25}
} asOrderedCollection.
people do: [:p | Transcript show:'{"name":"', (p at:#name), '","age":', (p at:#age) printString, '}'; cr].
