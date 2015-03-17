: emit-n 48 + emit ;
: foo
    if
        1
    else
        0
    then
    emit-n
;
0 0 foo emit-n
0 1 foo emit-n
0 2 foo emit-n
bye
