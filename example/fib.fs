: program ( cont args... func -- result... cont )
    begin dup while
    \ .s 10 emit
    dup 1 = if drop
        \ dup 1 <= if
        dup 0 = over 1 = + if
            \ r 0 1 -- 0 r
            \ r 1 1 -- 1 r
            swap
        else
            \ r n 1 -- r n-1 2 n-2 1 -- -- r n-1 (fib n-2) 2
            1- dup 1- 2 swap 1
        then
    else dup 2 = if drop
        \ r n m 2 -- r m 3 n 1 -- -- r m (fib n) 3
        3 rot 1
    else 3 = if
        \ r n m 3 -- n+m r
        + swap
    then then then
    repeat drop
;

: fib ( n -- n )
    0 swap 1 program
;

\ : fib' 0 1 rot 0 u+do .s 10 emit dup rot + loop drop ;

9 fib . cr
10 fib . cr
11 fib . cr

bye
