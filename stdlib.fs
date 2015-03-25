: . ( n -- )
    100 /mod
    dup if
        48 + emit
        1
    else
        drop
        0
    then
    swap
    10 /mod
    dup if
        48 + emit
        nip
    else
        drop
        swap if
            48 emit
        then
    then
    48 + emit
    space
;

: > ( a b -- bool )
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
: < swap > ;
: <= > 0= ;
: >= swap <= ;
