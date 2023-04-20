datatype lexresult = NUM of ExpOp.rational| NUMINT of BigInt.bigint | RATIONALDEC | INTEGERDEC | BOOLEANDEC | TRUE | FALSE | VARDEC
                    | IF | THEN | ELSE | FI | WHILE | DO | OD | PROCEDURE | PRINT | READ | CALL | INVERSE
                    | MAKERAT | RAT | SHOWRAT | SHOWDECIMAL | FROMDECIMAL | TODECIMAL | NOT | AND | OR
                    | ID of string| LCOMMENT | RCOMMENT | RATPLUS | RATSUB | RATTIMES | RATDIV | NEG | PLUS | SUB
                    | TIMES | DIV | MOD | LT | LE | GT | GE | EQ | NE | ASSIGN | LPAREN | RPAREN | LBRACE
                    | RBRACE | SEMI | COMMA | EOF;
val linenum = ref 1
val error = fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof = fn () => EOF
%%
%structure MyLex
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
alnum = [alpha|digit];
%%
\n => (linenum := !linenum + 1; lex());
{ws}+ => (lex());
{digit}*"."{digit}*"("{digit}+")" => (NUM (ExpOp.fromDecimal(yytext)));
{digit}+ => (NUMINT (BigInt.toBigint(yytext)));

rational=>(RATIONALDEC);
integer=>(INTEGERDEC);
boolean=>(BOOLEANDEC);
tt=> (TRUE);
ff=> (FALSE);
"var" => (VARDEC);
"if" => (IF);
"then" => (THEN);
"else" => (ELSE);
"fi" => (FI);
"while" => (WHILE);
"do" => (DO);
"od" => (OD);
"procedure" => (PROCEDURE);
"print" => (PRINT);
"read" => (READ);
"call" => (CALL);

"inverse" => (INVERSE);
"make_rat" => (MAKERAT);
"rat" => (RAT);
"showRat" => (SHOWRAT);
"showDecimal" => (SHOWDECIMAL);
"fromDecimal" => (FROMDECIMAL);
"toDecimal" => (TODECIMAL);


"!" => (NOT);
"&&" => (AND);
"||" => (OR);


{alpha}+{alnum}* => (ID(yytext));


"(*" => (LCOMMENT);
"*)" => (RCOMMENT);


".+." => (RATPLUS);
".-." => (RATSUB);
".*." => (RATTIMES);
"./." => (RATDIV);


"~" => (NEG);
"+" => (PLUS);
"-" => (SUB);
"*" => (TIMES);
"/" => (DIV);
"%" => (MOD);

"<" => (LT);
"<=" => (LE);
">" => (GT);
">=" => (GE);
"=" => (EQ);
"<>" => (NE);

":=" => (ASSIGN);

"(" => (LPAREN);
")" => (RPAREN);
"{" => (LBRACE);
"}" => (RBRACE);

";" => (SEMI);
"," => (COMMA);
