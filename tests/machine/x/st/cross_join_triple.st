| nums letters bools |
nums := {1. 2}.
letters := {'A'. 'B'}.
bools := {true. false}.
Transcript show: '--- Cross Join of three lists ---'; cr.
nums do: [:n |
  letters do: [:l |
    bools do: [:b |
      Transcript show: n printString, ' ', l, ' ', b printString; cr
    ]
  ]
].
