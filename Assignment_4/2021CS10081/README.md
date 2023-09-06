## Instructions to Run:
We need to run ``` CM.make "sources.cm" ``` in the REPL environment. Then, we need to run Calc.interpret() with the desired parameters
expressions. 
Here is the grammar for the language: 
1. Grammar for rational numbers 
    - **Start State**: R
    - **Terminals**: "~", "(", ")", ".", [0-9]
    - **Non Terminals**: R, D, N, P, Q
    - **Production Rules:** 
        - R -> D | ~D | D.D(N) | +D.D(N) | ~D.D(N)
        - D -> DP
        - N -> DQ
        - P -> eps | Q 
        - Q -> [0-9]
2. Grammar for rational number expressions ()
    - **Start State**: E
    - **Terminals**: ";", "+", "-", "*", "/",  ".", "(", ")", [0-9], "~"
    - **Non Terminals**: E, T, F, R, D, P, N, Q
    - **Production Rules:**
        - E -> E + T | E - T | T
        - T -> T + F | T / F | F
        - F -> R | (E)  
        - R -> D | ~D | D.D(N) | +D.D(N) | ~D.D(N)
        - D -> DP
        - N -> DQ
        - P -> eps | Q 
        - Q -> [0-9]
3. Overall grammar for rational numbers (incorporating both expressions and variables). Notation 
    - **Start State** : S
    - **Terminals** : "{", "}", "(", ")", [a-z], [A-Z], [0-9], "=", ".", ";", "+", "-", "*", "/", "~"
    - **Non Terminals** : S, B, E, A, C, V, E, T, F, R, D, N, P, Q
    - **Production Rules:**
        - S -> {B} E; | E;
        - B -> B , V = E
        - V -> A
        - A -> (the default alphabets) [A-Z a-z] C 
        - C -> eps | [A-Z | a-z | 0-9]
        - E -> E + T | E - T | T
        - T -> T + F | T / F | F
        - F -> R | (E)  
        - R -> D | ~D | D.D(N) | +D.D(N) | ~D.D(N)
        - D -> DP
        - N -> DQ
        - P -> eps | Q 
        - Q -> [0-9]


4. Overall grammar for rational-PL0
    - **Start State** : Program
    - **Terminals** : rational, integer, boolean, tt, ff, var, if, then, else, fi, while, do, od procedure, print,
    read, call,  ~, +, inverse, .+., .-., .*., ./. , make_rat, rat, showrat, showDecimal, fromDecimal, toDecimal , =, \<\>, <, <=, \>, \>=, :=, (, ), {, }, (\*, \*), !, ||, &&
    - **Non Terminals** :START, Block, DeclarationSeq, ratVarDecls, IntVarDecls, BoolVarDecls, ProcDecls, ProcDef ,CommandSeq CommandSeq1, CommandSeq2 ,Decs ,Commands ,Commands1 ,Commands2 ,Command ,AssignmentCmd ,CallCmd , ReadCmd ,PrintCmd ,ConditionalCmd ,WhileCmd ,Expression ,New1Expression ,New2Expression ,New3Expression ,New4Expression ,New5Expression ,New6Expression ,New7Expression, Var, VarTail
    - **Production Rules:** 
        - START : Block 

        - Block : DeclarationSeq ProcDecls { Commands } 

        - DeclarationSeq : ratVarDecls IntVarDecls BoolVarDecls 

        - ratVarDecls : eps | rational Decs ; 

        - IntVarDecls : eps
                    | integer Decs ;

        - BoolVarDecls  : eps
                    | boolean Decs ;

        - Decs : Var , Decs 
                | Var 

        - ProcDecls : eps
                | ProcDef ; ProcDecls 

        - ProcDef : procedure Var Block 


        - Commands : eps
                | Command ; Commands 

        - Command : Var := Expression 
                | call Var 
                | read ( Var ) 
                | print ( Expression ) 
                | if Expression then { Commands } else { Commands } fi 
                | while Expression do { Commands } od 

        - Expression: New1Expression 
                | Expression || New1Expression 

        - New1Expression: New2Expression 
                    | New1Expression && New2Expression 

        - New2Expression: New3Expression 
                    | New2Expression = New3Expression 
                    | New2Expression <> New3Expression  

        - New3Expression: New4Expression 
                    | New3Expression > New4Expression 
                    | New3Expression >= New4Expression 
                    | New3Expression < New4Expression 
                    | New3Expression <= New4Expression 

        - New4Expression: New5Expression 
                    | New4Expression + New5Expression 
                    | New4Expression - New5Expression  
                    | New4Expression .+. New5Expression 
                    | New4Expression .-. New5Expression 
                    
        - New5Expression: New6Expression
                    | New5Expression * New6Expression 
                    | New5Expression / New6Expression 
                    | New5Expression % New6Expression 
                    | New5Expression .*. New6Expression 
                    | New5Expression ./. New6Expression 

        - New6Expression: New7Expression
                    | inverse New6Expression
                    | rat New6Expression 
                    | showrat New6Expression 
                    | showDecimal New6Expression 
                    | fromDecimal New6Expression 
                    | toDecimal New6Expression 
                    | ~ New6Expression 
                    | ! New6Expression 

        - New7Expression: fromDecimal ( R ) 
                    | make_rat ( Expression , Expression ) 
                    | R
                    | tt 
                    | ff 
                    | ( Expression )
                    | Var 
    
        - Var: [A-Za-z]VarTail

        - VarTail: [A-Za-z0-9]VarTail | eps

NOTE: This R serves as the start state for the grammar for rational numbers

## Acknowledgements
The glue code, the lex and yacc files were taken from the original ML-yacc documentation, and the TextIO functions from the ML Lex-Yacc Guide by Roger Price, though the grammar and regex are my own, in addition to the original rational.sml and ast.sml's non-TextIO functions.

# Design Decisions
1. For numbers in decimal form, the user needs to put appropriate spaces between the preceding operator and the optional sign. 
2. Division by 0 raises an error
3. 0 has multiple representations in BigInt
4. The type bigint is implemented as a combination of an integer list and an integer (for the sign)
5. The type rational is a tuple of (bigint, bigint)
6. Signature rational was changed to incorporate more functions for ease of lexing and parsing
7. All undeclared identifiers are assigned value 0 or false
8. Any procedure A can call another procedure B iff any of A's ancestors in the AST has B as a child
9. fromDecimal may or may not take parentheses, and is optional to use in front of a standard decimal form. Makerat does take parentheses
10. Nested comments are not allowed, even though multi-line comments are
11. The input from stdIn is taken after a line has been printed to the console asking for that variable's value
12. Type checking is done at runtime
13. Undeclared bindings and procedures, redeclared bindings and procedures, and invalid inputs have been handled as errors.
