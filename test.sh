#!/usr/bin/bash

brainfuck-beef () {
    beef "$1"
}
brainfuck-bfopt () {
    local f g
    f=$(mktemp --suffix=.cpp)
    cat "$1" | python2 ~/lib/bfoptimization/optimizr.py > $f
    g=$(mktemp --suffix=.bin)
    g++ -O2 -o $g $f
    rm $f
    $g
    rm $g
}
brainfuck-bfc () {
    local f
    f=$(mktemp --suffix=.bin)
    cat "$1" | bfc > $f
    chmod +x $f
    $f
    rm $f
}
brainfuck () {
    brainfuck-beef "$@"
}

translate () {
    brainfuck <(runghc ForthToBrainfuck.hs)
}

error=0

assert () {
    echo assert \'"$1"\' \'"$2"\'
    if echo "$1" | translate | diff - <(echo -n "$2") ; then : ; else
        error=$(($error+1))
    fi
    echo
}
rv () {
    echo -n "$@" | rev
}
preserve () {
    local f
    f=$(mktemp)
    cat > $f
    echo preserve '<<'\''EOF'\'
    cat $f
    echo EOF
    if cat $f | translate | diff - <(gforth $f) ; then : ; else
        error=$(($error+1))
    fi
    rm $f
    echo
}

assert '48 emit' 0
assert '65 emit' A
assert '104 116 114 111 102 emit emit emit emit emit' forth
assert 'char A   emit' A
assert 'char Foo emit' F
assert 'char f emit char o emit char r emit char t emit char h emit' forth

assert '' ''
assert 'char A ( comment ) emit' A
assert 'char B emit \ comment' B
assert 'bye' ''

assert '30 35 + emit' A
assert '16  3 * emit' 0
assert '50  2 - emit' 0
assert '1 8 8 * + emit' A
assert 'char A 1+ 1+ emit' C
assert 'char Z 1- 1- emit' X

assert 'char A char B char C swap emit emit emit' $(rv ACB)
assert 'char A char B char C  rot emit emit emit' $(rv BCA)
assert 'char A char B char C -rot emit emit emit' $(rv CAB)
assert 'char A char B char C drop emit emit' $(rv AB)
assert 'char A char B char C  nip emit emit' $(rv AC)
assert 'char A char B  dup emit emit emit' $(rv ABB)
assert 'char A char B over emit emit emit' $(rv ABA)
assert 'char A char B tuck emit emit emit' $(rv BAB)

assert '7 0=  char 0 swap - emit' 0
assert '7 0<> char 0 swap - emit' 1
assert '0 0=  char 0 swap - emit' 1
assert '0 0<> char 0 swap - emit' 0
assert '32 32 =  char 0 swap - emit' 1
assert '32 27 =  char 0 swap - emit' 0
assert '32 32 <> char 0 swap - emit' 0
assert '32 27 <> char 0 swap - emit' 1

assert 'char A 7 if char t emit                  then emit' tA
assert 'char A 0 if char t emit                  then emit'  A
assert 'char A 7 if char t emit else char f emit then emit' tA
assert 'char A 0 if char t emit else char f emit then emit' fA

preserve <<'EOF'
48 emit
bye
EOF

preserve <<'EOF'
: foo ( n -- n^2+n+1 )
    dup dup * + 1+
;
4 foo emit
6 foo emit
bye
EOF

preserve <<'EOF'
: foo ( n -- 2*n )
    dup +
;
: bar ( n -- 4*n )
    foo foo
;
: baz ( n -- 16*n )
    bar foo bar
;
1 foo emit
2 foo emit
3 foo emit
bye
EOF

preserve <<'EOF'
: emit-n 48 + emit ;
: foo
    dup emit-n
    1+  emit-n
;
8 foo
bye
EOF

preserve <<'EOF'
: emit-n 48 + emit ;
: foo
    begin dup while
        dup emit-n
        1-
    repeat
;
6 7 8 foo emit-n emit-n emit-n
bye
EOF

preserve <<'EOF'
: emit-n 48 + emit ;
: foo
    if
        1 emit-n
    else
        0 emit-n
    then
;
0 0 foo emit-n
0 1 foo emit-n
0 2 foo emit-n
bye
EOF

preserve <<'EOF'
123 .
103 .
120 .
100 .
72 .
34 .
12 .
10 .
8 .
2 .
1 .
0 .
bye
EOF

preserve <<'EOF'
: foo
    10 swap
    case
        0 of 48 emit endof
        1 of 49 emit endof
        2 of 50 emit endof
        48 + emit 0
    endcase
    emit
;
0 foo
1 foo
2 foo
3 foo
4 foo
bye
EOF

preserve <<'EOF'
bl
65 emit
10 space
cr
space
bye
EOF

preserve <<'EOF'
10 66 . emit
bye
EOF

preserve <<'EOF'
: foo
    case
    0 of 65 emit endof
    1 of 66 emit endof
    2 of 67 emit endof
    3 of 68 emit endof
    4 of 69 emit endof
    97 + dup emit
    endcase
;
10 0 foo emit
10 1 foo emit
10 2 foo emit
10 3 foo emit
10 4 foo emit
10 5 foo emit
10 6 foo emit
10 7 foo emit
bye
EOF

preserve <<'EOF'
: foo
    case
    0 of 65 endof
    1 of 66 endof
    2 of 67 endof
    3 of 68 endof
    4 of 69 endof
    97 + dup
    endcase
;
10 0 foo emit emit
10 1 foo emit emit
10 2 foo emit emit
10 3 foo emit emit
10 4 foo emit emit
10 5 foo emit emit
10 6 foo emit emit
10 7 foo emit emit
bye
EOF

preserve <<'EOF'
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
EOF

preserve <<'EOF'
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
EOF

preserve <<'EOF'
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
EOF

for i in $(seq 0 127) ; do
    assert "$i ." "$i "
done

for a in $(seq 0 10) ; do
    for b in $(seq 0 10) ; do
        echo "$a $b + . bye" | preserve
        echo "$a $b * . bye" | preserve
    done
done

for a in $(seq 0 15) ; do
    for b in $(seq 0 $a) ; do
        echo "$a $b - . bye" | preserve
    done
done

for a in $(seq 0 15) ; do
    for b in $(seq 1 15) ; do
        echo "$a $b /mod . . bye" | preserve
    done
done

echo $error errors found
exit $error
