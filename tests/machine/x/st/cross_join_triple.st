| bools combos letters nums |
nums := {1. 2}.
letters := {'A'. 'B'}.
bools := {true. false}.
combos := [ | tmp |
  tmp := OrderedCollection new.
  nums do: [:n |
    letters do: [:l |
      bools do: [:b |
        tmp add: Dictionary newFrom: {n -> n. l -> l. b -> b}.
      ].
    ].
  ].
  tmp
] value.
Transcript show: '--- Cross Join of three lists ---'; cr.
combos do: [:c |
  Transcript show: (c.n) printString; show: ' '; show: (c.l) printString; show: ' '; show: (c.b) printString; cr.
].
