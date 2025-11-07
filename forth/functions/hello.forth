: HELLO ( -- ) ." Hello, Forth!" CR ;
: SQUARE ( n -- n^2 ) DUP * ;
: FACTORIAL ( n -- n! )
    DUP 2 < IF DROP 1 EXIT THEN
    DUP 1- FACTORIAL * ;
HELLO
5 SQUARE . CR
5 FACTORIAL . CR
