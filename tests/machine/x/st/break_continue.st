| n numbers |
Object subclass: #BreakSignal instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!.
Object subclass: #ContinueSignal instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!.
numbers := {1. 2. 3. 4. 5. 6. 7. 8. 9}.
[.
numbers do: [:n |.
[.
(((n % 2) = 0)) ifTrue: [
ContinueSignal signal.
].
((n > 7)) ifTrue: [
BreakSignal signal.
].
Transcript show: 'odd number:'; show: ' '; show: (n) printString; cr.
] on: ContinueSignal do: [:ex | ].
].
] on: BreakSignal do: [:ex | ].
