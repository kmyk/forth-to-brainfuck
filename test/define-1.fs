: foo ( n -- n^2+n+1 )
    dup dup * + 1+
;
4 foo emit
6 foo emit
bye
