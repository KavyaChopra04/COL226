structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token
val pos = ref 0
fun eof () = Tokens.EOF(!pos,!pos)
%%
%header (functor CalcLexFun(structure Tokens: Calc_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
alnum = [alpha|digit];
%%
\n => (pos := !pos + 1; lex());
{ws}+ => (lex());
{digit}*"."{digit}*"("{digit}+")" => (Tokens.NUMRAT((ExpOp.fromDecimal(yytext)), !pos, !pos));
{digit}+ => (Tokens.NUMINT ((BigInt.toBigint(yytext)), !pos, !pos));

"rational" => (Tokens.RATIONALDEC(!pos,!pos));
"integer" => (Tokens.INTEGERDEC(!pos,!pos));
"boolean" => (Tokens.BOOLEANDEC(!pos,!pos));
"tt" => (Tokens.TRUE(!pos,!pos));
"ff" => (Tokens.FALSE(!pos,!pos));
"var" => (Tokens.VARDEC(!pos,!pos));
"if" => (Tokens.IF(!pos,!pos));
"then" => (Tokens.THEN(!pos,!pos));
"else" => (Tokens.ELSE(!pos,!pos));
"fi" => (Tokens.FI(!pos,!pos));
"while" => (Tokens.WHILE(!pos,!pos));
"do" => (Tokens.DO(!pos,!pos));
"od" => (Tokens.OD(!pos,!pos));
"procedure" => (Tokens.PROCEDURE(!pos,!pos));
"print" => (Tokens.PRINT(!pos,!pos));
"read" => (Tokens.READ(!pos,!pos));
"call" => (Tokens.CALL(!pos,!pos));

"inverse" => (Tokens.INVERSE(!pos,!pos));
"make_rat" => (Tokens.MAKERAT(!pos,!pos));
"rat" => (Tokens.RAT(!pos,!pos));
"showRat" => (Tokens.SHOWRAT(!pos,!pos));
"showDecimal" => (Tokens.SHOWDECIMAL(!pos,!pos));
"fromDecimal" => (Tokens.FROMDECIMAL(!pos,!pos));
"toDecimal" => (Tokens.TODECIMAL(!pos,!pos));

"!" => (Tokens.NOT(!pos,!pos));
"&&" => (Tokens.AND(!pos,!pos));
"||" => (Tokens.OR(!pos,!pos));

{alpha}+{alnum}* => (Tokens.ID(yytext,!pos,!pos));

"(*" => (Tokens.LCOMMENT(!pos,!pos));
"*)" => (Tokens.RCOMMENT(!pos,!pos));

".+." => (Tokens.RATPLUS(!pos,!pos));
".-." => (Tokens.RATSUB(!pos,!pos));
".*." => (Tokens.RATTIMES(!pos,!pos));
"./." => (Tokens.RATDIV(!pos,!pos));

"~" => (Tokens.NEG(!pos,!pos));
"+" => (Tokens.PLUS(!pos,!pos));
"-" => (Tokens.SUB(!pos,!pos));
"*" => (Tokens.TIMES(!pos,!pos));
"/" => (Tokens.DIV(!pos,!pos));
"%" => (Tokens.MOD(!pos,!pos));


"<" => (Tokens.LT(!pos,!pos));
"<=" => (Tokens.LE(!pos,!pos));
">" => (Tokens.GT(!pos,!pos));
">=" => (Tokens.GE(!pos,!pos));
"=" => (Tokens.EQ(!pos,!pos));
"<>" => (Tokens.NE(!pos,!pos));


":=" => (Tokens.ASSIGN(!pos,!pos));


"(" => (Tokens.LPAREN(!pos,!pos));
")" => (Tokens.RPAREN(!pos,!pos));
"{" => (Tokens.LBRACE(!pos,!pos));
"}" => (Tokens.RBRACE(!pos,!pos));


";" => (Tokens.SEMI(!pos,!pos));
"," => (Tokens.COMMA(!pos,!pos));
