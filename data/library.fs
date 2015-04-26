: 1+ s` +` ;
: 1- s` -` ;

: + s` [<+>-]<` ;
: - s` [<->-]<` ;
: * s` <[>[>+>+<<-]>>[<<+>>-]<<<-]>[-]>[<<+>>-]<<` ;

: drop ( w -- )                 s` [-]<` ;
: nip  ( w1 w2 -- w2 )          s` <[-]>[<+>-]<` ;
: dup  ( w -- w w )             s` [>+>+<<-]>>[<<+>>-]<` ;
: over ( w1 w2 -- w1 w2 w1 )    s` <[>>+>+<<<-]>>>[<<<+>>>-]<` ;
: tuck ( w1 w2 -- w2 w1 w2 )    s` [>+>+<<-]<[>+<-]>>>[<<<+>>>-]<` ;
: swap ( w1 w2 -- w2 w1 )       s` [>+<-]<[>+<-]>>[<<+>>-]<` ;
: rot  ( w1 w2 w3 -- w2 w3 w1 ) s` <<[>>>+<<<-]>[<+>-]>[<+>-]>[<+>-]<` ;
: -rot ( w1 w2 w3 -- w3 w1 w2 ) s` [>+<-]<[>+<-]<[>+<-]>>>[<<<+>>>-]<` ;
: dup? ( w -- 0 | w w ) dup if dup then ;

: 2drop ( w1 w2 -- ) drop drop ;
: 2nip  ( w1 w2 w3 w4 -- w3 w4 ) rot drop rot drop ;
: 2dup  ( w1 w2 -- w1 w2 w1 w2 ) over over ;
: 2over ( w1 w2 w3 w4 -- w1 w2 w3 w4 w1 w2 ) 3 pick 3 pick ;
: 2tuck ( w1 w2 w3 w4 -- w3 w4 w1 w2 w3 w4 ) 3 roll 3 roll 3 pick 3 pick ;
: 2swap ( w1 w2 w3 w4 -- w3 w4 w1 w2 ) 3 roll 3 roll ;
: 2rot  ( w1 w2 w3 w4 w5 w6 -- w3 w4 w5 w6 w1 w2 ) 5 roll 5 roll ;

: negate 0 swap - ;
: under+ rot + swap ;

: emit   s` .` drop ;
: key  0 s` ,` ;

: 0=  s` >-<[>+<[-]]>[<->+]<` ;
: 0<>    s` [>-<[-]]>[<->+]<` ;
: =  - 0=  ;
: <> - 0<> ;

: /mod
    over
    if
        \ http://sampi.hatenablog.com/entry/2013/10/01/001136
        s` [>>+<<-]>+>>>>>+[<<<<[->+>+<<]>[-<+>]>[-<<<<<-[>]>>>>>>>-<<<<<<[<]>>>>]>+>]<<<<<<<[>]>[>[-<<<+>>>]>>>-<<<<<]>->[-]>>>>>[-]<<[<<<<<+>>>>>-]<<<<<`
    else
        s` [-]`
    then
;
: /   /mod nip  ;
: mod /mod drop ;

: max
    \ http://sampi.hatenablog.com/entry/2013/09/28/115426
    s` [>>+<<-]>+>>>+>>++[<<<<<<<-[>]>>>>>>>-<<<<<<[<]>>-[>]>>>>-<<<[<]>>+>]>[-]<<<-<<[-]<-<<[-]>>>>>>[<<<<<<+>>>>>>-]<<<<<<`
;
: min
    s` [>>+<<-]>+>>>+>>+[<<<<<<<-[>]>>>>>>>[-]<<<<<<[<]>>-[>]>>>>[-]<<<[<]>>+>]<<-<<[-]<->>>>[<<<<<<+>>>>>>-]<<<<<<`
;

: cr 10 emit ;
: bl 32 ;
: space  bl emit ;
: spaces bl s` <[>.<-]>[-]<<` ;

: dec. ( n -- )
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
: . dec. ;

: > ( a b -- f )
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

: 0<  0 <  ;
: 0<= 0 <= ;
: 0>  0 >  ;
: 0>= 0 >= ;

: true -1 ;
: false 0 ;

\ not in forth
: +of ( n1 n2 -- n3 f ) \ + with overflow flag (0 or -1)
    0 -rot \ 0 n1 n2
    begin dup while
        \ swap 1+ dup \ f n2 n1' n1'
        \ if else rot 1- -rot then \ f n2 n1
        \ swap 1- \ f n1 n2'
        s` <+[>>+>+<<<-]>>>` \ f 0 n2 n1' n1'
        s` [<<<<+>>>>[-]]<<<<->>>` \ f' 0 n2 n1
        s` [<<+>>-]<-` \ f n1 n2'
    repeat \ f n1 0
    drop swap \ n1 f
;
\ not in forth
: -uf ( n1 n2 -- n3 f ) \ - with underflow flag (0 or -1)
    0 -rot
    begin dup while
        swap dup if else rot 1- -rot then 1-
        swap 1-
    repeat
    drop swap
;

: and' ( f1 f2 -- f3 ) \ not in forth
    if 0<> else drop 0 then
;
: or' ( f1 f2 -- f3 ) \ not in forth
    if drop -1 else 0<> then
;
: not' ( f1 -- f2 ) \ not in forth
    0=
;

: within ( u1 u2 u3 - f )
    2 pick > -rot >= and'
;

: d+ ( d1 d2 -- d3 )
    swap \ a1 a2 b2 b1
    3 roll +of negate \ a2 b2 c1 f'
    2 roll + \ a2 c1 b2'
    2 roll + \ c1 c2
;

: d- ( d1 d2 -- d3 )
    swap
    3 roll swap -uf
    2 roll swap -
    2 roll swap -
;

: dnegate ( d1 -- d2 )
    0 0 2swap d-
;

: d0= ( d1 d2 -- f )
    0= swap 0= and'
;
: d0<> ( d1 d2 -- f )
    0<> swap 0<> or'
;

: m* ( n1 n2 -- n1 f )
    0 -rot 0 -rot \ 0 0 n1 n2
    begin dup while \ f a b i
        rot 2 pick +of \ f b i a' f
        \ if 3 roll 1+ 3 roll 3 roll 3 roll then \ f' b i a
        s` [<<<<+>>>>+]<`
        -rot 1- \ f a b i
    repeat drop \ f a b
    drop swap \ a f
;
