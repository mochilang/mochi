| people |
people := {Dictionary newFrom: {name -> 'Alice'. age -> 30}. Dictionary newFrom: {name -> 'Bob'. age -> 25}}.
(Main _save: people path: '-' opts: Dictionary newFrom: {format -> 'jsonl'}).
