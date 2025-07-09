| numbers |
numbers := {1. 2. 3. 4. 5. 6. 7. 8. 9}.
numbers do: [:n |
  (((n % 2) = 0)) ifTrue: [
    " continue"
  ] .
  ((n > 7)) ifTrue: [
    " break"
  ] .
  Transcript show: 'odd number:'; show: ' '; show: (n) printString; cr.
].
