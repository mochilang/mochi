| nums letters bools combos c |
nums := {1. 2}.
letters := {'A'. 'B'}.
bools := {true. false}.
combos := [ | tmp |
  tmp := OrderedCollection new.
  nums do: [:n |
    letters do: [:l |
      bools do: [:b |
        tmp add: Dictionary newFrom:{n->n. l->l. b->b}.
      ].
    ].
  ].
  tmp
] value.
Transcript show: '--- Cross Join of three lists ---'; cr.
combos do: [:c |.
Transcript show: (c at: 'n') printString; show: ' '; show: (c at: 'l') printString; show: ' '; show: (c at: 'b') printString; cr.
].
.
