" Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
"
| writeTwo appendOneMore main lines |
writeTwo := [ {'jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash'. 'jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jsmith:/bin/bash'}. ].
appendOneMore := [:lines | lines copyWith: 'xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash'. ].
main := [ lines := writeTwo value.
lines := appendOneMore value: lines.
((((((lines size)) >= 3)) and: [(((lines at: 2) = ('xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash')))])) ifTrue: [
Transcript show: 'append okay'; cr.
] ifFalse: [
Transcript show: 'it didn''t work'; cr.
]. ].
main value.
