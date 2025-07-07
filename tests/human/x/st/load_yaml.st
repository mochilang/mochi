| people adults |
people := {
  Dictionary newFrom:{#name->'Alice'. #age->30. #email->'alice@example.com'}.
  Dictionary newFrom:{#name->'Bob'. #age->15. #email->'bob@example.com'}.
  Dictionary newFrom:{#name->'Charlie'. #age->20. #email->'charlie@example.com'}
} asOrderedCollection.

adults := people select: [:p | (p at:#age) >= 18] collect: [:p | Dictionary newFrom:{#name->(p at:#name). #email->(p at:#email)}].
adults do: [:a | Transcript show:(a at:#name); space; show:(a at:#email); cr].
