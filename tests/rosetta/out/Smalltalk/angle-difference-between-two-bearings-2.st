" Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
"
| angleDiff diff testCases tc |
angleDiff := [:b1 :b2 | diff := (b2 - b1).
(((((((((diff % 360)) + 360)) + 180)) % 360)) - 180). ].
testCases := {{20. 45}. {(0 - 45). 45}. {(0 - 85). 90}. {(0 - 95). 90}. {(0 - 45). 125}. {(0 - 45). 145}. {29.4803. (0 - 88.6381)}. {(0 - 78.3251). (0 - 159.036)}. {(0 - 70099.74233810938). 29840.67437876723}. {(0 - 165313.6666297357). 33693.9894517456}. {1174.8380510598456. (0 - 154146.66490124757)}. {60175.77306795546. 42213.07192354373}}.
testCases do: [:tc |.
Transcript show: (angleDiff value: tc at: 0 value: tc at: 1) printString; cr.
].
.
