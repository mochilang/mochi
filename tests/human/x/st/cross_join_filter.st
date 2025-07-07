| nums letters |
nums := {1. 2. 3}.
letters := {'A'. 'B'}.
Transcript show: '--- Even pairs ---'; cr.
nums do: [:n |
  (n even) ifTrue: [
    letters do: [:l | Transcript show: n printString, ' ', l; cr ]
  ]
].
