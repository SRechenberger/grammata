grammata 
========

**grammata** is a script language interpreted and run on a monadic virtual machine.
It supports polyparadigmatic features such as 
  * imperative procedures and functions 
  * functional expressions 
  * logical knowledge bases and queries to use them

This project, in particular, the virtual Machine, was part of the Bachelor Thesis
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
then initiate a cabal sandbox and build with cabal
```
cabal install
```
then, if not already done, add `~/.cabal/bin` to your `$PATH` variable.

Usage
-----

to run a script, simply call, for example
```
grammata examples/quicksort.gr
```
it will then be compiled and executed.
you can either accept the printed solution by pressing the 'y' button, 
or demand a new one, by pressing an other one.

Examples
--------

Example programs are included in the `examples` folder.

Copyright
---------
(c) 2014, 2015, Sascha Rechenberger
licensed under GNU Public License Version 3