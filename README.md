grammata 
========

**grammata** is a polyparadigmatic script language interpreted and run on a monadic virtual machine. 
It supports the *imperative* as well as the *functional* and *logical* programming paradigm, combining them in a *procedural* manner.
It can be seen as a partly extended partly constrained external representation of the internal machine language,
to which it is compiled.

This project, in particular, the virtual Machine, was part of the bachelor thesis
"Das Grammateion - Monadische Implementierung einer virtuellen Maschine zur Ausführung einer polyparadigmatischen Programmiersprache"
("The Grammateion - Monadic Implementation of a virtual machine for the execution of a polyparadigmatic programming language")
by Sascha Rechenberger.


Installation
------------

First, clone from GitHub 
```
git clone git@github.com:SRechenberger/grammata.git
cd grammata
```
Then build with cabal
```
cabal install
```
If not already done, add `~/.cabal/bin` to your `$PATH` variable.

Usage
-----

To run a script, simply call, for example
```
grammata examples/quicksort.gr
```
It will then be compiled and executed,
producing the output 
```
cons(2,cons(5,cons(6,cons(7,cons(8,cons(22,nil)))))) ?
```
You can either accept the printed solution by pressing the 'y' key, 
or demand the next one by pressing any other one.
In this case there is only one solution, so in either case it terminates with `OK.`

Examples
--------

Example programs are included in the `examples` folder.

Copyright
---------
(c) 2014, 2015, Sascha Rechenberger
licensed under GNU Public License Version 3
