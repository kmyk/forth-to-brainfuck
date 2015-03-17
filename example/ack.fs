: program ( cont args... func -- result... cont )
    begin dup while
    \ .s 10 emit
    dup 1 = if drop
        swap dup 0= if
            \ r 0 n 1 -- n+1 r
            drop 1+ swap
        else swap dup 0= if
            \ r m 0 1 -- r m-1 1 1
            drop 1- 1 1
        else
            \ r m n 1 -- r m-1 1 m n-1 1
            swap dup 1- rot rot 1 swap rot 1- 1
        then then
    then
    repeat drop
;

: ack ( m n -- ack )
    0 -rot 1 program
;

2 8 ack . cr
3 2 ack . cr
4 0 ack . cr

( : ack' { m n }
    m 0= if
        n 1+
    else n 0= if
        m 1- 1 recurse
    else
        m 1- m n 1- recurse recurse
    then then
; )

bye
