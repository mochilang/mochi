" Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
"
| creature pointer |
creature := 'shark'.
pointer := {creature}.
Transcript show: ((('creature = ') + creature)) printString; cr.
Transcript show: 'pointer = 0'; cr.
Transcript show: ((('*pointer = ') + (pointer at: 0))) printString; cr.
pointer at: 0 put: 'jellyfish'.
creature := pointer at: 0.
Transcript show: ((('*pointer = ') + (pointer at: 0))) printString; cr.
Transcript show: ((('creature = ') + creature)) printString; cr.
