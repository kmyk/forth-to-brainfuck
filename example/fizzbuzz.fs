: emit-fizz 102 emit 105 emit 122 emit 122 emit ;
: emit-buzz  98 emit 117 emit 122 emit 122 emit ;
: fizzbuzz ( -- )
    1 begin dup while
        dup 15 mod 0= if
            emit-fizz emit-buzz
        else dup 5 mod 0= if
            emit-buzz
        else dup 3 mod 0= if
            emit-fizz
        else
            dup .
        then then then
        cr
        dup 100 = if
            drop 0
        else
            1+
        then
    repeat
;
fizzbuzz
bye
