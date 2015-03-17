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
