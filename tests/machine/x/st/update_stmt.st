| people expected |
people := {
  Dictionary newFrom:{#name->'Alice'. #age->17. #status->'minor'}.
  Dictionary newFrom:{#name->'Bob'. #age->25. #status->'unknown'}.
  Dictionary newFrom:{#name->'Charlie'. #age->18. #status->'unknown'}.
  Dictionary newFrom:{#name->'Diana'. #age->16. #status->'minor'}
} asOrderedCollection.

people do: [:p |
  (p at:#age) >= 18 ifTrue:[
    p at:#status put:'adult'.
    p at:#age put:(p at:#age)+1
  ]
].

expected := {
  Dictionary newFrom:{#name->'Alice'. #age->17. #status->'minor'}.
  Dictionary newFrom:{#name->'Bob'. #age->26. #status->'adult'}.
  Dictionary newFrom:{#name->'Charlie'. #age->19. #status->'adult'}.
  Dictionary newFrom:{#name->'Diana'. #age->16. #status->'minor'}
} asOrderedCollection.

(people = expected) ifTrue:[Transcript show:'ok'; cr] ifFalse:[Transcript show:'fail'; cr].
