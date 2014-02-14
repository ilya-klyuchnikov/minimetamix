## `foetus` in Scala

### What is `foetus`?

* [Andreas Abel. `foetus` – termination checker for simple functional programs. Programming Lab Report (1998)](http://www.tcs.informatik.uni-muenchen.de/~abel/foetus.pdf)
* [Andreas Abel and Thorsten Altenkirch. A predicative analysis of structural recursion. JFP 12.1 (2002)](http://www2.tcs.ifi.lmu.de/~abel/foetuswf.pdf)

### Quick start

    $ sbt test:run

    [info] Running foetus.test.examples
    ======================
    checking id0
    id0 PASSES termination check
    ======================
    checking loop
    loop FAILS termination check
    ======================
    checking natid
    natid PASSES termination check (by lexical order 0)
    ======================
    checking natid1
    natid1 FAILS termination check
    ======================
    checking add
    add PASSES termination check (by lexical order 0)
    ======================
    checking y
    y PASSES termination check
    ======================
    checking listid
    listid PASSES termination check (by lexical order 0)
    ======================
    checking add, mult
    add PASSES termination check (by lexical order 0)
    mult PASSES termination check (by lexical order 0)
    ======================
    checking pred, sub
    pred PASSES termination check
    sub PASSES termination check (by lexical order 0)
    ======================
    checking pred, sub, div
    pred PASSES termination check
    sub PASSES termination check (by lexical order 0)
    div PASSES termination check
    div1 FAILS termination check
    ======================
    checking ack
    ack PASSES termination check (by lexical order 0, 1)
    ======================
    checking ack1
    ack1 FAILS termination check
    ======================
    checking map
    map PASSES termination check
    map1 PASSES termination check (by lexical order 0)
    ======================
    checking foldl
    foldl PASSES termination check
    foldl1 PASSES termination check (by lexical order 0, 1)
    ======================
    checking nil, cons, foldl, rev
    nil PASSES termination check
    cons PASSES termination check
    foldl PASSES termination check
    foldl1 PASSES termination check (by lexical order 0, 1)
    rev PASSES termination check
    ======================
    checking nil, cons, flatten
    nil PASSES termination check
    cons PASSES termination check
    flatten FAILS termination check
    ======================
    checking nil, cons, flat1, aux
    nil PASSES termination check
    cons PASSES termination check
    flat1 PASSES termination check (by lexical order 0)
    aux PASSES termination check (by lexical order 0, 1, 0)
