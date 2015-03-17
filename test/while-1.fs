: emit-n 48 + emit ;
: foo
    begin dup while
        dup emit-n
        1-
    repeat
;
6 7 8 foo emit-n emit-n emit-n
bye
