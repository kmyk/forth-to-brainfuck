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
