: p ( -- a b ) 10 33 ;
: q ( a b c -- ) 48 swap - emit emit emit ;
: l 108 emit ;
: g 103 emit ;
: e 101 emit ;
: t 116 emit ;

l e p 3 7 <= q
l t p 3 7 <  q
g e p 3 7 >= q
g t p 3 7 >  q
cr
l e p 4 4 <= q
l t p 4 4 <  q
g e p 4 4 >= q
g t p 4 4 >  q
cr
l e p 8 12 <= q
l t p 8 12 <  q
g e p 8 12 >= q
g t p 8 12 >  q
cr
l e p 255 255 <= q
l t p 255 255 <  q
g e p 255 255 >= q
g t p 255 255 >  q
cr
l e p 254 255 <= q
l t p 254 255 <  q
g e p 254 255 >= q
g t p 254 255 >  q
cr
l e p 255 254 <= q
l t p 255 254 <  q
g e p 255 254 >= q
g t p 255 254 >  q

bye
