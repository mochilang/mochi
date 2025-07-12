| xs ys m s |
xs := {1. 2. 3}.
ys := [ | tmp |
  tmp := OrderedCollection new.
  xs do: [:x |
    (((x % 2) = 1)) ifTrue: [
      tmp add: x.
    ].
  ].
  tmp
] value.
Transcript show: ((1 in ys)) printString; cr.
Transcript show: ((2 in ys)) printString; cr.
m := Dictionary newFrom:{'a'->1}.
Transcript show: (('a' in m)) printString; cr.
Transcript show: (('b' in m)) printString; cr.
s := 'hello'.
Transcript show: (('ell' in s)) printString; cr.
Transcript show: (('foo' in s)) printString; cr.
