| people |
people := {Dictionary newFrom: {#name->'Alice'. #age->17. #status->'minor'}. Dictionary newFrom: {#name->'Bob'. #age->25. #status->'unknown'}. Dictionary newFrom: {#name->'Charlie'. #age->18. #status->'unknown'}. Dictionary newFrom: {#name->'Diana'. #age->16. #status->'minor'}}.
people do: [:it |.
((age >= 18)) ifTrue: [.
it at: 'status' put: 'adult'.
it at: 'age' put: (age + 1).
].
].
((people = {Dictionary newFrom: {#name->'Alice'. #age->17. #status->'minor'}. Dictionary newFrom: {#name->'Bob'. #age->26. #status->'adult'}. Dictionary newFrom: {#name->'Charlie'. #age->19. #status->'adult'}. Dictionary newFrom: {#name->'Diana'. #age->16. #status->'minor'}})) ifTrue: [Transcript show:'ok'; cr] ifFalse: [Transcript show:'fail'; cr].
Transcript show: ('ok') printString; cr.
