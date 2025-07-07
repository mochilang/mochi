| union diff inter len |
union := ({1. 2} asSet union: {2. 3} asSet) asArray.
diff := {1. 2. 3} copyWithout: 2.
inter := {1. 2. 3} select: [:x | {2. 4} includes: x].
len := ({1. 2} , {2. 3}) size.
Transcript show: union printString; cr.
Transcript show: diff printString; cr.
Transcript show: inter printString; cr.
Transcript show: len printString; cr.
