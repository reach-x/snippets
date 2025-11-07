USING: io math sequences ;
IN: hello

: hello ( -- ) "Hello from Factor!" print ;

: factorial ( n -- n! )
    dup 2 < [ drop 1 ] [ dup 1 - factorial * ] if ;

hello
{ 1 2 3 4 5 } [ . ] each
5 factorial .
