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
