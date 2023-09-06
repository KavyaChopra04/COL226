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
alnum = [alpha | digit];
%%
\n => (pos := !pos + 1; lex());
{ws}+ => (lex());
{digit}*"."{digit}*"("{digit}+")" => (Tokens.NUM ((ExpOp.fromDecimal(yytext)), !pos, !pos));
"~"{digit}*"."{digit}*"("{digit}+")" => (Tokens.NUM ((ExpOp.fromDecimal(yytext)), !pos, !pos));
"+"{digit}*"."{digit}*"("{digit}+")" => (Tokens.NUM ((ExpOp.fromDecimal(yytext)), !pos, !pos));
"~"{digit}+ => (Tokens.NUM ((ExpOp.fromInteger(yytext)), !pos, !pos));
{digit}+ => (Tokens.NUM ((ExpOp.fromInteger(yytext)), !pos, !pos));
{alpha}+{alnum}* => (Tokens.ID(yytext,!pos,!pos)); 
"+" => (Tokens.PLUS(!pos,!pos));
"-" => (Tokens.SUB(!pos,!pos));
"*" => (Tokens.TIMES(!pos,!pos));
"/" => (Tokens.DIV(!pos,!pos));
";" => (Tokens.SEMI(!pos,!pos));
"(" => (Tokens.LPAREN(!pos,!pos));
")" => (Tokens.RPAREN(!pos,!pos));

"{" => (Tokens.LBRACE(!pos,!pos));
"}" => (Tokens.RBRACE(!pos,!pos));
"," => (Tokens.COMMA(!pos,!pos));
"=" => (Tokens.EQ(!pos,!pos));
