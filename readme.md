# Forth to Brainf\*ck

## how to use

``` sh
    $ cat forth.fs | forth-to-brainfuck > brainfuck.bf
```

## example

``` sh
    $ cat fizzbuzz.fs
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

    $ cat fizzbuzz.fs | runhaskell ForthToBrainfuck.hs > fizzbuzz.bf

    $ head -c 80 fizzbuzz.bf
    >+[[>+>+<<-]>>[<<+>>-]<>+++++++++++++++<[>>+>+<<<-]>>>[<<<+>>>-]<>+<[[-]>-<<[>>+

    $ wc -c fizzbuzz.bf
    3899 fizzbuzz.bf

    $ /path/to/interpreter fizzbuzz.bf | tail
    91 
    92 
    fizz
    94 
    buzz
    fizz
    97 
    98 
    fizz
    buzz
```

## todo

-   support some multiple length arithmetic
-   support `recurse` or `recursive`

## license

MIT License
