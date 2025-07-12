| nums letters pairs p |
nums := {1. 2. 3}.
letters := {'A'. 'B'}.
pairs := [ | tmp |
  tmp := OrderedCollection new.
  nums do: [:n |
    letters do: [:l |
      (((n % 2) = 0)) ifTrue: [
        tmp add: Dictionary newFrom:{n->n. l->l}.
      ].
    ].
  ].
  tmp
] value.
Transcript show: '--- Even pairs ---'; cr.
pairs do: [:p |.
Transcript show: (p at: 'n') printString; show: ' '; show: (p at: 'l') printString; cr.
].
.
