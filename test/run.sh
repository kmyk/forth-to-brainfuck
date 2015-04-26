#!/usr/bin/bash

simple=t
if [ "$simple" ] ; then
    simple=$(mktemp --suffix=.bin)
    clang -o "$simple" interpreter.c
fi
brainfuck-simple () {
    # EOF is -1 (fgetc)
    # memory is char[30000], not a loop
    "$simple" "$@"
}
brainfuck-bf () {
    bf "$1"
}
brainfuck-beef () {
    # on my environment, it is 7bit
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
    # on my environment, it sometimes throw sigsegv
    local f
    f=$(mktemp --suffix=.bin)
    cat "$1" | bfc > $f
    chmod +x $f
    $f
    rm $f
}
brainfuck () {
    brainfuck-simple "$@"
}

if brainfuck <(echo "-.") | hexdump | diff - <(python -c 'import sys; sys.stdout.buffer.write(b"\xff")' | hexdump) ; then : ; else
    echo 'your brainfuck interpreter is not 8bit'
    exit 1
fi

bin=../dist/build/forth-to-brainfuck/forth-to-brainfuck
if not which $bin ; then
    bin=forth-to-brainfuck
fi
translate () {
    brainfuck <($bin)
}

error=$(mktemp)

assert () {
    echo assert \'"$1"\' \'"$2"\'
    if echo "$1" | translate | diff - <(echo -n "$2") ; then : ; else
        echo assert \'"$1"\' \'"$2"\' >> $error
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
        echo preserve $f >> $error
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

for f in *.fs ; do
    cat "$f" | preserve
done

for f in ../example/*.fs ; do
    cat "$f" | preserve
done

if false ; then
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
fi

if [ "$simple" ] ; then
    rm "$simple"
fi

returncode=$(cat $error | wc -l)
echo $(cat $error | wc -l) errors found
cat $error
rm $error
exit $returncode
