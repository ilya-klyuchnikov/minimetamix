## minimetamix - a collection of simple and minimalistic items.

- `common`
  - a minimalistic parser implemented via scala macros
- `foetus`. simple termination checker
  - [Andreas Abel. `foetus` – termination checker for simple functional programs. Programming Lab Report (1998)](http://www.tcs.informatik.uni-muenchen.de/~abel/foetus.pdf)
  - [Andreas Abel and Thorsten Altenkirch. A predicative analysis of structural recursion. JFP 12.1 (2002)](http://www2.tcs.ifi.lmu.de/~abel/foetuswf.pdf)
- `sll` (WIP)
  - experiments on minimalistic interpreters

### Quick start

```
$ sbt test
```

### SLL nameless interpreters

```
$ sbt test:console
> print(sll.pprinter.pprintProgram(sll.test.nameless.Nameless0Q.program))
iEval(IVal(v)) = v;
iEval(IFCall0(n)) = dEval0([getF0(n)]);
iEval(IGCall1(n, farg1)) = switchVal(farg1, n);
switchVal(Ctr0(pn), gn) = dEval0([getG00(gn, pn)]);
switchVal(Ctr1(pn, arg1), gn) = Err();
switchVal(Ctr2(pn, arg1, arg2), gn) = Err();
switchVal(Err(), gn) = Err();
dEval0(DCtr0(n)) = Ctr0(n);
dEval0(DCtr10(n, arg)) = Ctr1(n, dEval0(arg));
dEval0(DCtr200(n, arg1, arg2)) = Ctr2(n, dEval0(arg1), dEval0(arg2));
dEval0(DFCall0(n)) = dEval0([getF0(n)]);
> print(sll.pprinter.pprintProgram(sll.test.nameless.Nameless1Q.program))

```
