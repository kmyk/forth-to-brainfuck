\ they are built in as: >= <= > <

: ge ( n m -- n >= m )
    begin dup while
        over
        if
            swap 1- swap
        then
        1-
    repeat
    drop
    0<>
;

: le ( n m -- n <= m )
    swap ge
;

: gt ( n m -- n >= m )
    over over <> if
        ge
    else
        -1
    then
;

: lt ( n m -- n >= m )
    swap gt
;

\ the . cannot print -1
: o ( n -- ) 48 swap - emit ;

3 7 le o
3 7 lt o
3 7 ge o
3 7 gt o

4 4 le o
4 4 lt o
4 4 ge o
4 4 gt o

8 12 le o
8 12 lt o
8 12 ge o
8 12 gt o

255 255 le o
255 255 lt o
255 255 ge o
255 255 gt o

254 255 le o
254 255 lt o
254 255 ge o
254 255 gt o

255 254 le o
255 254 lt o
255 254 ge o
255 254 gt o

bye
