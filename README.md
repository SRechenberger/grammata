grammata
========

**Grammata** is a simple script language interpreted and run as a monadic structure.

Syntax
------

Every *program* has the stucture `program { <DECLARAION>* <STATEMENT>* }`,
whereas 
* **`DECLARATION`** is 
  * an uninitialized number `num <ID>;`.
  * an initialize number `num <ID> := <NUMBER>;`.
  * a function `func <ID> := func ( <PARAM>* ) { <DECLARATION>* <STATEMENT>* };`.

* **`STATEMENT`** is
  * an assignment `<ID> := <NUMBER>;`
  * a for loop `for( <COUNTER> ; <END> ; <STEP> ) { <STATEMENT>* }`, whereas
    * **`COUNTER`** is the `ID` of the counter variable.
    * the value of the `EXPRESSION` **`END`** must be less (if `STEP` < 0) or greater (if `STEP` >= 0) than `COUNTER` to continue the loop.
    * the value of the `EXPRESSION` **`STEP`** is added to the counter variable after every iteration.
  * a while loop `while ( <COND> ) { <STATEMENT>* };` runs till the `EXPRESSION` `COND` is less then 0.
  * a do while loop `do { <STATEMENT>* } while ( <COND> );` runs till the `EXPRESSION` `COND` is less then 0. The body will be executed at least once.
  * a one armed if `if ( <COND> ) then { <STATEMENT>* };`. The then block will be executed if the `EXPRESSION` `COND` is greater or equal to 0.
  * a two armed if `if ( <COND> ) then { <STATEMENT>* } else { <STATEMENT>* };`. The then block will be executed if the `EXPRESSION` `COND` is greater or equal to 0, the else block otherwise.
  * a return `return <TORETURN>`, which will either print the result of the program or return a value to the calling function; however, the value of the `EXPRESSION` `TORETURN` is returned.
* **`ID`** is an identifier beginning with a lower or uppercase letter and consisting of lower and uppercase letters and ciphers.
* **`NUMBER`** is a number literal, consisting of two blocks of ciphers seprated by a `.` (`123.456`) or one block of ciphers (`123`).
* a **`PARAM`** has the form `num <ID>` or `func <ID>` (higher order functions are **not** implemented yet) separated by `,`.
* an **`EXPRESSION`** is 
  * an identifiers `ID`.
  * a constants `NUMBER`.
  * a binary operations `<EXPRESSION> ° <EXPRESSION>` whereas `°` is 
    * an addition `+`.
    * a subtraction `-`.
    * a multiplication `*`.
    * a division `/`.
    * an integer division `div`.
    * a modulo operation `%` (returns integer values).
    * a comparisons `<`, `>`, `<=`, `>=`, `==` and `!=` which yield `-1` if as `false` and `1` as `true`.
  * a unary `-<EXPRESSION>` (can be used to negate logically).
  * a function applications `<ID>( <ARG>* )` whereas `ARG` is an `EXPRESSION`.

`<PARAM>*` and `<ARG>*` are separated by `,`.

Comments are 
* single line comments initialized with `##`.
* mulit line comments surrounded by `# ... #`.

Examples
--------

Example programs are included in the `examples` folder.
