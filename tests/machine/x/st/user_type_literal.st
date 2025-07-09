| book |
book := Dictionary newFrom: {#title->'Go'. #author->Dictionary newFrom: {#name->'Bob'. #age->42}}.
Transcript show: (book at: 'author' at: 'name') printString; cr.
