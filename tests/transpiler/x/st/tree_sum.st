" Code generated by Mochi transpiler on 2025-07-21 23:34 +0700
t := Node newFrom: {
  'left'->'Leaf'.
  'right'->Node newFrom: {
    'left'->'Leaf'.
    'right'->'Leaf'.
    'value'->2
  }.
  'value'->1
}.
Transcript show:3; cr
