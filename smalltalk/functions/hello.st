"Smalltalk example"
Transcript show: 'Hello from Smalltalk!'; cr.

numbers := #(1 2 3 4 5).
numbers do: [:each | Transcript show: each printString; cr].

factorial := [:n | n <= 1 ifTrue: [1] ifFalse: [n * (factorial value: n - 1)]].
Transcript show: (factorial value: 5) printString; cr.
