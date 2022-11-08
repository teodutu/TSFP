Files:
* Syntax
    * Parser.hs: the parsing library
    * Expression.hs: TODO: the internal expression representation
    * Grammar.hs: TODO: the language grammar
* Evaluation
    * Substitution.hs: TODO: substitution rules and free variables
    * Normal.hs: TODO: rules for small-step normal-order evaluation
    * Applicative.hs: TODO: rules for small-step applicative-order evaluation
    * Big.hs: TODO: big-step evaluation of single expressions and expression
        lists
* Typing
    * Type.hs: internal type representation
    * Unification.hs: TODO: type unification
    * Inference.hs: TODO: type inference
* Main.hs: main module
* prog.txt: sample program

See the tests for use cases.

The tests make the following assumptions:
* The data type that encodes lambda expressions instantiates the 'Show' class.
* The Grammar module exposes the 'parseProgram' function, which takes
  a program string and returns a list of internal expression representations. 
* The Substitution module exposes
    * the 'subst' function, which encodes the substitution rules
    * the 'freeVars' function, which returns the list of free variable names
      within a given expression.
* In case of conflict, the 'subst' function renames the bound variable
  by appending a '#' to its name.