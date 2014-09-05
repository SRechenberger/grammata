grammata 
========

**grammata** is a simple script language interpreted and run on a monadic virtual machine.
It supports polyparadigmatic features such as 
  * imperative procedures and functions 
  * functional expressions 
  * logical knowledge bases and queries to use them

Syntax
------

### Grammata Programs
```
PROGRAM ::= program {A..Z}{a..z|A..Z|0..9} { with DECL* }? begin SUBPRG+ end
DECL    ::= var IDENT { := EXPRESSION }? ;
IDENT      ::= {a..z}{ 0..9 | A..Z | a..z }*
SUBPRG  ::= FUNCTIONAL 
          | IMPERATIVE 
          | QUERY 
          | BASE
EXPRESSION ::= DISJ { || DISJ}*
DISJ ::= CONJ { && CONJ}*
CONJ ::= COMP {{ == | != | <= | >= | < | > } COMP}*
COMP ::= SUM {{ + | - } SUM}*
SUM ::= FAC {{ * | / } FAC}*
FAC ::= ( EXPRESSION )
      | { - | ! } EXPRESSION
      | IDENT{(EXPRESSION { , EXPRESSION}*)}?
      | PARAM_VALUE
```
### Functional
```
FUNCTIONAL ::= lambda IDENT ( IDENT* ) is LAMBDA end
LAMBDA     ::= ARITH ARITH*
ARITH      ::= DISJ {|| DISJ}*
DISJ       ::= KONJ {&& KONJ}*
KONJ       ::= COMP {{ == | != | <= | >= | < | > } COMP}*
COMP       ::= SUM {{+ | -} SUM}*
SUM        ::= SIMPLE {{ * | / } SIMPLE}*
SIMPLE     ::= \\ LOG+ . LAMBDA
             | if LAMBDA then LAMBDA else LAMBDA end
             | let DEF* in LAMBDA end
             | ( LAMBDA )
             | LOG
             | VALUE
             | IDENT[( LAMBDA [, LAMBDA]* )]
DEF        ::= LOG := LAMBDA ;
LOG        ::= $IDENT 
```
### Imperative
```
IMPERATIVE ::= { func | proc } IDENT ( {IDENT { , IDENT }*}? ) { with DECL+ }? does STMT* end
STMT       ::= for IDENT { from EXPRESSION }? to EXPRESSION { in EXPRESSION }? do STMT* end
             | while EXPRESSION to STMT* end
             | do STMT* while EXPRESSION end
             | if EXPRESSION then STMT* else STMT* end
             | call IDENT({ EXPRESSION { , EXPRESSION }*}?) ;
             | IDENT := EXPRESSION ;
             | return EXPRESSION ; 
             | exit ; 
             | backtrack ;
```
### Logical
```
QUERY ::= query ( { IDENT { , IDENT}*}? ) { asks IDENT* } { for IDENT } ?- CLAUSE end
CLAUSE ::= DISJ { ; DISJ}*
DISJ   ::= CONJ { , CONJ}*
CONJ   ::= - CLAUSE 
         | ( CLAUSE )
         | GOAL 
GOAL   ::= TERM :=: TERM 
         | IDENT{( TERM { , TERM }*)}?
TERM   ::= VALUE 
         | {A..Z}{a..z|A..Z|0..9}*
         | EXPRESSION 
BASE   ::= base IDENT says RULE+ end
RULE   ::= GOAL { :- CLAUSE}? .
```

Examples
--------

Example programs are included in the `examples` folder.
