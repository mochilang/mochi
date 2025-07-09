| people |
people := {Dictionary newFrom:{#name->'Alice'. #age->30}. Dictionary newFrom:{#name->'Bob'. #age->25}}.
save value: people value: "-" value: Dictionary newFrom:{#format->'jsonl'}.
