datatype lexresult= NUM of string | PLUS | SUB | TIMES | DIV | SEMI | LPAREN | RPAREN | LBRACE | RBRACE | COMMA | EQ | ASSGN | ID of string | EOF 
val linenum = ref 1
val error = fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof = fn () => EOF
fun toint (x : char list) = 
    if(null(x)) then 0
    else (ord(hd(x)) - ord(#"0")) + 10 * toint(tl(x))
%%
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
alnum = [alpha | digit];
%%
\n => (linenum := !linenum + 1; lex());
{ws}+ => (lex());
"~"{digit}+"."{digit}*"("{digit}+")" => (NUM yytext);
"+"{digit}+"."{digit}*"("{digit}+")" => (NUM yytext);
{digit}+"."{digit}*"("{digit}+")" => (NUM yytext);
"~"{digit}+ => (NUM yytext);
{digit}+ => (NUM yytext);
{alpha}+{alnum}* => (if yytext="val" then (ASSGN) else (ID yytext)); 
"+" => (PLUS);
"-" => (SUB);
"*" => (TIMES);
"/" => (DIV);
";" => (SEMI);
"(" => (LPAREN);
"{" => (LBRACE);
"}" => (RBRACE);
"," => (COMMA);
"=" => (EQ);
