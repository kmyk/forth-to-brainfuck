: program ( cont args... func -- result... cont )
    begin dup while
3 pick .
2 pick .
1 pick .
0 pick .
cr
        case
            1 of 2 pick 2 pick <= if \ ret x y z --
                drop nip swap else   \ y ret \ if x <= y
                2 then endof         \ ret x y z 2 \ otherwise
            2 of 3 3 pick 1- 3 pick 3 pick 1 endof \ ret x y z     -- ret x y z 3 x-1 y z 1
            3 of 4 3 pick 1- 3 pick 6 pick 1 endof \ ret x y z a   -- ret x y z a 4 y-1 z x 1
            4 of 5 3 pick 1- 6 pick 6 pick 1 endof \ ret x y z a b -- ret x y z a b 5 z-1 x y 1
            5 of 3 roll drop 3 roll drop 3 roll drop 1 endof \ ret x y z a b c -- ret a b c 1
        endcase
    repeat drop
;

: tarai ( x y z -- n )
    0 3 roll 3 roll 3 roll 1 program
;

: t0 ( x y z -- n )
    -rot over over <= if
        nip nip
    else rot over over <= if
        nip nip
    else
        drop drop
    then then
;

9 2 7 t0 .
8 4 2 t0 .
7 9 3 t0 .
cr

9 2 7 tarai .
8 4 2 tarai .
7 9 3 tarai .
cr

bye
