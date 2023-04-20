functor CalcLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Calc_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* Sample interactive calculator for ML-Yacc *)
open DataTypes
val ListofBindings: (string * ExpOp.rational) list ref = ref [];
exception UndeclaredBinding
fun intlistToString []      = ""
  | intlistToString [(x,y)]     = x^" : "^ExpOp.showDecimal(y)
  | intlistToString ((x,y)::xs) = x^" : "^ExpOp.showDecimal(y)^ " , " ^ intlistToString xs 

fun finserval(id, xs) = 
  if(null(xs)) then 
    (print ("Invalid Expression: No binding found for " ^ id ^ "\n");
    raise UndeclaredBinding;
    ExpOp.fromInteger("0"))
  else 
  (
    let  
      val (id1, expression) = hd(xs)
    in 
      if(String.compare(id1, id) = EQUAL) then 
        expression
      else 
        finserval(id, tl(xs))
    end)
fun addBinding(id, exp, xs) = 
       ( xs := (id, exp) :: !xs ; xs)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\013\000\000\000\
\\001\000\001\000\019\000\000\000\
\\001\000\001\000\038\000\000\000\
\\001\000\001\000\062\000\002\000\061\000\003\000\060\000\008\000\059\000\
\\009\000\058\000\021\000\057\000\023\000\056\000\024\000\055\000\
\\025\000\054\000\026\000\053\000\027\000\052\000\028\000\051\000\
\\029\000\050\000\050\000\049\000\000\000\
\\001\000\001\000\062\000\002\000\061\000\003\000\060\000\008\000\059\000\
\\009\000\058\000\050\000\049\000\000\000\
\\001\000\001\000\066\000\000\000\
\\001\000\011\000\095\000\031\000\084\000\000\000\
\\001\000\012\000\124\000\000\000\
\\001\000\013\000\128\000\000\000\
\\001\000\015\000\085\000\031\000\084\000\000\000\
\\001\000\016\000\123\000\000\000\
\\001\000\031\000\084\000\050\000\098\000\000\000\
\\001\000\031\000\084\000\051\000\117\000\000\000\
\\001\000\049\000\064\000\000\000\
\\001\000\050\000\039\000\000\000\
\\001\000\050\000\040\000\000\000\
\\001\000\051\000\097\000\000\000\
\\001\000\052\000\018\000\000\000\
\\001\000\052\000\116\000\000\000\
\\001\000\052\000\118\000\000\000\
\\001\000\052\000\125\000\000\000\
\\001\000\053\000\037\000\000\000\
\\001\000\053\000\121\000\000\000\
\\001\000\053\000\122\000\000\000\
\\001\000\053\000\127\000\000\000\
\\001\000\054\000\017\000\000\000\
\\001\000\054\000\020\000\000\000\
\\001\000\054\000\023\000\000\000\
\\001\000\054\000\035\000\000\000\
\\001\000\054\000\036\000\000\000\
\\001\000\056\000\000\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\133\000\004\000\006\000\000\000\
\\134\000\000\000\
\\135\000\005\000\008\000\000\000\
\\136\000\000\000\
\\137\000\006\000\015\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\055\000\021\000\000\000\
\\141\000\017\000\011\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\001\000\032\000\010\000\031\000\014\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\000\000\
\\145\000\000\000\
\\146\000\031\000\084\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\000\000\
\\151\000\000\000\
\\152\000\030\000\083\000\000\000\
\\153\000\030\000\083\000\000\000\
\\154\000\047\000\082\000\048\000\081\000\000\000\
\\155\000\047\000\082\000\048\000\081\000\000\000\
\\156\000\043\000\080\000\044\000\079\000\045\000\078\000\046\000\077\000\000\000\
\\157\000\043\000\080\000\044\000\079\000\045\000\078\000\046\000\077\000\000\000\
\\158\000\043\000\080\000\044\000\079\000\045\000\078\000\046\000\077\000\000\000\
\\159\000\034\000\076\000\035\000\075\000\038\000\074\000\039\000\073\000\000\000\
\\160\000\034\000\076\000\035\000\075\000\038\000\074\000\039\000\073\000\000\000\
\\161\000\034\000\076\000\035\000\075\000\038\000\074\000\039\000\073\000\000\000\
\\162\000\034\000\076\000\035\000\075\000\038\000\074\000\039\000\073\000\000\000\
\\163\000\034\000\076\000\035\000\075\000\038\000\074\000\039\000\073\000\000\000\
\\164\000\036\000\072\000\037\000\071\000\040\000\070\000\041\000\069\000\
\\042\000\068\000\000\000\
\\165\000\036\000\072\000\037\000\071\000\040\000\070\000\041\000\069\000\
\\042\000\068\000\000\000\
\\166\000\036\000\072\000\037\000\071\000\040\000\070\000\041\000\069\000\
\\042\000\068\000\000\000\
\\167\000\036\000\072\000\037\000\071\000\040\000\070\000\041\000\069\000\
\\042\000\068\000\000\000\
\\168\000\036\000\072\000\037\000\071\000\040\000\070\000\041\000\069\000\
\\042\000\068\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\"
val actionRowNumbers =
"\034\000\036\000\042\000\031\000\
\\000\000\038\000\000\000\025\000\
\\017\000\001\000\026\000\041\000\
\\033\000\000\000\027\000\042\000\
\\045\000\034\000\035\000\000\000\
\\028\000\037\000\043\000\029\000\
\\021\000\002\000\014\000\015\000\
\\003\000\003\000\013\000\044\000\
\\040\000\039\000\045\000\032\000\
\\048\000\005\000\003\000\076\000\
\\070\000\065\000\060\000\057\000\
\\055\000\053\000\009\000\003\000\
\\004\000\004\000\004\000\004\000\
\\004\000\004\000\004\000\004\000\
\\088\000\087\000\086\000\085\000\
\\090\000\006\000\003\000\046\000\
\\016\000\011\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\018\000\
\\012\000\084\000\083\000\082\000\
\\081\000\080\000\079\000\078\000\
\\077\000\019\000\047\000\049\000\
\\050\000\073\000\072\000\071\000\
\\075\000\074\000\067\000\066\000\
\\069\000\068\000\062\000\061\000\
\\064\000\063\000\059\000\058\000\
\\056\000\054\000\045\000\089\000\
\\045\000\022\000\023\000\010\000\
\\007\000\052\000\020\000\045\000\
\\024\000\008\000\051\000\030\000"
val gotoT =
"\
\\001\000\127\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\005\000\005\000\000\000\
\\007\000\008\000\008\000\007\000\000\000\
\\000\000\
\\012\000\010\000\000\000\
\\006\000\012\000\000\000\
\\012\000\014\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\020\000\000\000\
\\000\000\
\\007\000\022\000\008\000\007\000\000\000\
\\013\000\024\000\016\000\023\000\000\000\
\\002\000\031\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\012\000\032\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\023\000\046\000\024\000\045\000\025\000\044\000\026\000\043\000\
\\027\000\042\000\028\000\041\000\029\000\040\000\030\000\039\000\000\000\
\\023\000\061\000\024\000\045\000\025\000\044\000\026\000\043\000\
\\027\000\042\000\028\000\041\000\029\000\040\000\030\000\039\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\063\000\016\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\023\000\065\000\024\000\045\000\025\000\044\000\026\000\043\000\
\\027\000\042\000\028\000\041\000\029\000\040\000\030\000\039\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\023\000\084\000\024\000\045\000\025\000\044\000\026\000\043\000\
\\027\000\042\000\028\000\041\000\029\000\040\000\030\000\039\000\000\000\
\\030\000\085\000\000\000\
\\030\000\086\000\000\000\
\\030\000\087\000\000\000\
\\030\000\088\000\000\000\
\\030\000\089\000\000\000\
\\030\000\090\000\000\000\
\\030\000\091\000\000\000\
\\030\000\092\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\023\000\094\000\024\000\045\000\025\000\044\000\026\000\043\000\
\\027\000\042\000\028\000\041\000\029\000\040\000\030\000\039\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\029\000\097\000\030\000\039\000\000\000\
\\029\000\098\000\030\000\039\000\000\000\
\\029\000\099\000\030\000\039\000\000\000\
\\029\000\100\000\030\000\039\000\000\000\
\\029\000\101\000\030\000\039\000\000\000\
\\028\000\102\000\029\000\040\000\030\000\039\000\000\000\
\\028\000\103\000\029\000\040\000\030\000\039\000\000\000\
\\028\000\104\000\029\000\040\000\030\000\039\000\000\000\
\\028\000\105\000\029\000\040\000\030\000\039\000\000\000\
\\027\000\106\000\028\000\041\000\029\000\040\000\030\000\039\000\000\000\
\\027\000\107\000\028\000\041\000\029\000\040\000\030\000\039\000\000\000\
\\027\000\108\000\028\000\041\000\029\000\040\000\030\000\039\000\000\000\
\\027\000\109\000\028\000\041\000\029\000\040\000\030\000\039\000\000\000\
\\026\000\110\000\027\000\042\000\028\000\041\000\029\000\040\000\
\\030\000\039\000\000\000\
\\026\000\111\000\027\000\042\000\028\000\041\000\029\000\040\000\
\\030\000\039\000\000\000\
\\025\000\112\000\026\000\043\000\027\000\042\000\028\000\041\000\
\\029\000\040\000\030\000\039\000\000\000\
\\024\000\113\000\025\000\044\000\026\000\043\000\027\000\042\000\
\\028\000\041\000\029\000\040\000\030\000\039\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\117\000\016\000\023\000\000\000\
\\000\000\
\\013\000\118\000\016\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\124\000\016\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 128
val numrules = 60
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUMINT of unit ->  (BigInt.bigint)
 | NUMRAT of unit ->  (ExpOp.rational) | ID of unit ->  (string)
 | New7Expression of unit ->  (EXP) | New6Expression of unit ->  (EXP)
 | New5Expression of unit ->  (EXP) | New4Expression of unit ->  (EXP)
 | New3Expression of unit ->  (EXP) | New2Expression of unit ->  (EXP)
 | New1Expression of unit ->  (EXP) | Expression of unit ->  (EXP)
 | WhileCmd of unit ->  (CMD) | ConditionalCmd of unit ->  (CMD)
 | PrintCmd of unit ->  (CMD) | ReadCmd of unit ->  (CMD)
 | CallCmd of unit ->  (CMD) | AssignmentCmd of unit ->  (CMD)
 | Command of unit ->  (CMD) | Commands2 of unit ->  (CMD list)
 | Commands1 of unit ->  (CMD list) | Commands of unit ->  (CMD list)
 | Decs of unit ->  (string list) | CommandSeq2 of unit ->  (CMD list)
 | CommandSeq1 of unit ->  (CMD list)
 | CommandSeq of unit ->  (CMD list) | ProcDef of unit ->  (PROCDEF)
 | ProcDecls of unit ->  (PROCDEF list)
 | BoolVarDecls of unit ->  (DEC) | IntVarDecls of unit ->  (DEC)
 | RatVarDecls of unit ->  (DEC)
 | DeclarationSeq of unit ->  (DEC list) | Block of unit ->  (BLOCK)
 | START of unit ->  (AST)
end
type svalue = MlyValue.svalue
type result = AST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 55) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "NUMRAT"
  | (T 2) => "NUMINT"
  | (T 3) => "RATIONALDEC"
  | (T 4) => "INTEGERDEC"
  | (T 5) => "BOOLEANDEC"
  | (T 6) => "VARDEC"
  | (T 7) => "TRUE"
  | (T 8) => "FALSE"
  | (T 9) => "IF"
  | (T 10) => "THEN"
  | (T 11) => "ELSE"
  | (T 12) => "FI"
  | (T 13) => "WHILE"
  | (T 14) => "DO"
  | (T 15) => "OD"
  | (T 16) => "PROCEDURE"
  | (T 17) => "PRINT"
  | (T 18) => "READ"
  | (T 19) => "CALL"
  | (T 20) => "INVERSE"
  | (T 21) => "MAKERAT"
  | (T 22) => "RAT"
  | (T 23) => "SHOWRAT"
  | (T 24) => "SHOWDECIMAL"
  | (T 25) => "FROMDECIMAL"
  | (T 26) => "TODECIMAL"
  | (T 27) => "NEG"
  | (T 28) => "NOT"
  | (T 29) => "AND"
  | (T 30) => "OR"
  | (T 31) => "LCOMMENT"
  | (T 32) => "RCOMMENT"
  | (T 33) => "RATPLUS"
  | (T 34) => "RATSUB"
  | (T 35) => "RATTIMES"
  | (T 36) => "RATDIV"
  | (T 37) => "PLUS"
  | (T 38) => "SUB"
  | (T 39) => "TIMES"
  | (T 40) => "DIV"
  | (T 41) => "MOD"
  | (T 42) => "LT"
  | (T 43) => "LE"
  | (T 44) => "GT"
  | (T 45) => "GE"
  | (T 46) => "EQ"
  | (T 47) => "NE"
  | (T 48) => "ASSIGN"
  | (T 49) => "LPAREN"
  | (T 50) => "RPAREN"
  | (T 51) => "LBRACE"
  | (T 52) => "RBRACE"
  | (T 53) => "SEMI"
  | (T 54) => "COMMA"
  | (T 55) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 55) $$ (T 54) $$ (T 53) $$ (T 52) $$ (T 51) $$ (T 50) $$ (T 49)
 $$ (T 48) $$ (T 47) $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42)
 $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35)
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28)
 $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14)
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Block Block1, Block1left, Block1right)) :: 
rest671)) => let val  result = MlyValue.START (fn _ => let val  (Block
 as Block1) = Block1 ()
 in (PROG(Block))
end)
 in ( LrTable.NT 0, ( result, Block1left, Block1right), rest671)
end
|  ( 1, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.Commands 
Commands1, _, _)) :: _ :: ( _, ( MlyValue.ProcDecls ProcDecls1, _, _))
 :: ( _, ( MlyValue.DeclarationSeq DeclarationSeq1, 
DeclarationSeq1left, _)) :: rest671)) => let val  result = 
MlyValue.Block (fn _ => let val  (DeclarationSeq as DeclarationSeq1) =
 DeclarationSeq1 ()
 val  (ProcDecls as ProcDecls1) = ProcDecls1 ()
 val  (Commands as Commands1) = Commands1 ()
 in (BLOCK(DeclarationSeq, ProcDecls, Commands))
end)
 in ( LrTable.NT 1, ( result, DeclarationSeq1left, RBRACE1right), 
rest671)
end
|  ( 2, ( ( _, ( MlyValue.BoolVarDecls BoolVarDecls1, _, 
BoolVarDecls1right)) :: ( _, ( MlyValue.IntVarDecls IntVarDecls1, _, _
)) :: ( _, ( MlyValue.RatVarDecls RatVarDecls1, RatVarDecls1left, _))
 :: rest671)) => let val  result = MlyValue.DeclarationSeq (fn _ =>
 let val  (RatVarDecls as RatVarDecls1) = RatVarDecls1 ()
 val  (IntVarDecls as IntVarDecls1) = IntVarDecls1 ()
 val  (BoolVarDecls as BoolVarDecls1) = BoolVarDecls1 ()
 in (RatVarDecls :: IntVarDecls :: BoolVarDecls :: [])
end)
 in ( LrTable.NT 2, ( result, RatVarDecls1left, BoolVarDecls1right), 
rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.RatVarDecls (fn _ =>
 ((DEC([], RATIONAL))))
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.Decs Decs1, _,
 _)) :: ( _, ( _, RATIONALDEC1left, _)) :: rest671)) => let val  
result = MlyValue.RatVarDecls (fn _ => let val  (Decs as Decs1) = 
Decs1 ()
 in (print("adding rational bindings"); DEC(Decs, RATIONAL))
end)
 in ( LrTable.NT 3, ( result, RATIONALDEC1left, SEMI1right), rest671)

end
|  ( 5, ( rest671)) => let val  result = MlyValue.IntVarDecls (fn _ =>
 (DEC([], INT)))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 6, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.Decs Decs1, _,
 _)) :: ( _, ( _, INTEGERDEC1left, _)) :: rest671)) => let val  result
 = MlyValue.IntVarDecls (fn _ => let val  (Decs as Decs1) = Decs1 ()
 in (DEC(Decs, INT))
end)
 in ( LrTable.NT 4, ( result, INTEGERDEC1left, SEMI1right), rest671)

end
|  ( 7, ( rest671)) => let val  result = MlyValue.BoolVarDecls (fn _
 => (DEC([], BOOL)))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 8, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.Decs Decs1, _,
 _)) :: ( _, ( _, BOOLEANDEC1left, _)) :: rest671)) => let val  result
 = MlyValue.BoolVarDecls (fn _ => let val  (Decs as Decs1) = Decs1 ()
 in (DEC(Decs, BOOL))
end)
 in ( LrTable.NT 5, ( result, BOOLEANDEC1left, SEMI1right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.Decs Decs1, _, Decs1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.Decs (fn _ => let val  (ID as ID1) = ID1 ()
 val  (Decs as Decs1) = Decs1 ()
 in ((ID :: Decs))
end)
 in ( LrTable.NT 11, ( result, ID1left, Decs1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.Decs (fn _ => let val  (ID as ID1) = ID1
 ()
 in (([ID]))
end)
 in ( LrTable.NT 11, ( result, ID1left, ID1right), rest671)
end
|  ( 11, ( rest671)) => let val  result = MlyValue.ProcDecls (fn _ =>
 (([])))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ProcDecls ProcDecls1, _, ProcDecls1right))
 :: _ :: ( _, ( MlyValue.ProcDef ProcDef1, ProcDef1left, _)) :: 
rest671)) => let val  result = MlyValue.ProcDecls (fn _ => let val  (
ProcDef as ProcDef1) = ProcDef1 ()
 val  (ProcDecls as ProcDecls1) = ProcDecls1 ()
 in (ProcDef :: ProcDecls)
end)
 in ( LrTable.NT 6, ( result, ProcDef1left, ProcDecls1right), rest671)

end
|  ( 13, ( ( _, ( MlyValue.Block Block1, _, Block1right)) :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, PROCEDURE1left, _)) :: rest671))
 => let val  result = MlyValue.ProcDef (fn _ => let val  (ID as ID1) =
 ID1 ()
 val  (Block as Block1) = Block1 ()
 in (PROCDEF(ID, Block))
end)
 in ( LrTable.NT 7, ( result, PROCEDURE1left, Block1right), rest671)

end
|  ( 14, ( rest671)) => let val  result = MlyValue.Commands (fn _ => (
([])))
 in ( LrTable.NT 12, ( result, defaultPos, defaultPos), rest671)
end
|  ( 15, ( ( _, ( MlyValue.Commands Commands1, _, Commands1right)) ::
 _ :: ( _, ( MlyValue.Command Command1, Command1left, _)) :: rest671))
 => let val  result = MlyValue.Commands (fn _ => let val  (Command as 
Command1) = Command1 ()
 val  (Commands as Commands1) = Commands1 ()
 in (Command :: Commands)
end)
 in ( LrTable.NT 12, ( result, Command1left, Commands1right), rest671)

end
|  ( 16, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let
 val  result = MlyValue.Command (fn _ => let val  (ID as ID1) = ID1 ()
 val  (Expression as Expression1) = Expression1 ()
 in ((SET(ID, Expression)))
end)
 in ( LrTable.NT 15, ( result, ID1left, Expression1right), rest671)

end
|  ( 17, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( _, 
CALL1left, _)) :: rest671)) => let val  result = MlyValue.Command (fn
 _ => let val  (ID as ID1) = ID1 ()
 in ((Call(ID)))
end)
 in ( LrTable.NT 15, ( result, CALL1left, ID1right), rest671)
end
|  ( 18, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ID ID1, _, _
)) :: _ :: ( _, ( _, READ1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (ID as ID1) = ID1 ()
 in ((Read(ID)))
end)
 in ( LrTable.NT 15, ( result, READ1left, RPAREN1right), rest671)
end
|  ( 19, ( ( _, ( _, _, LPAREN2right)) :: ( _, ( MlyValue.Expression 
Expression1, _, _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) =>
 let val  result = MlyValue.Command (fn _ => let val  (Expression as 
Expression1) = Expression1 ()
 in ((Print(Expression)))
end)
 in ( LrTable.NT 15, ( result, PRINT1left, LPAREN2right), rest671)
end
|  ( 20, ( ( _, ( _, _, FI1right)) :: _ :: ( _, ( MlyValue.Commands 
Commands2, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.Commands Commands1
, _, _)) :: _ :: _ :: ( _, ( MlyValue.Expression Expression1, _, _))
 :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 val  Commands1 = Commands1 ()
 val  Commands2 = Commands2 ()
 in ((IFFI(Expression,Commands1,Commands2)))
end)
 in ( LrTable.NT 15, ( result, IF1left, FI1right), rest671)
end
|  ( 21, ( ( _, ( _, _, OD1right)) :: _ :: ( _, ( MlyValue.Commands 
Commands1, _, _)) :: _ :: _ :: ( _, ( MlyValue.Expression Expression1,
 _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result =
 MlyValue.Command (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 val  (Commands as Commands1) = Commands1 ()
 in (WHILE(Expression,Commands))
end)
 in ( LrTable.NT 15, ( result, WHILE1left, OD1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.New1Expression New1Expression1, 
New1Expression1left, New1Expression1right)) :: rest671)) => let val  
result = MlyValue.Expression (fn _ => let val  (New1Expression as 
New1Expression1) = New1Expression1 ()
 in ((New1Expression))
end)
 in ( LrTable.NT 22, ( result, New1Expression1left, 
New1Expression1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.New1Expression New1Expression1, _, 
New1Expression1right)) :: _ :: ( _, ( MlyValue.Expression Expression1,
 Expression1left, _)) :: rest671)) => let val  result = 
MlyValue.Expression (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 val  (New1Expression as New1Expression1) = New1Expression1 ()
 in ((OR(Expression, New1Expression)))
end)
 in ( LrTable.NT 22, ( result, Expression1left, New1Expression1right),
 rest671)
end
|  ( 24, ( ( _, ( MlyValue.New2Expression New2Expression1, 
New2Expression1left, New2Expression1right)) :: rest671)) => let val  
result = MlyValue.New1Expression (fn _ => let val  (New2Expression as 
New2Expression1) = New2Expression1 ()
 in ((New2Expression))
end)
 in ( LrTable.NT 23, ( result, New2Expression1left, 
New2Expression1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.New2Expression New2Expression1, _, 
New2Expression1right)) :: _ :: ( _, ( MlyValue.New1Expression 
New1Expression1, New1Expression1left, _)) :: rest671)) => let val  
result = MlyValue.New1Expression (fn _ => let val  (New1Expression as 
New1Expression1) = New1Expression1 ()
 val  (New2Expression as New2Expression1) = New2Expression1 ()
 in ((AND(New1Expression, New2Expression)))
end)
 in ( LrTable.NT 23, ( result, New1Expression1left, 
New2Expression1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.New3Expression New3Expression1, 
New3Expression1left, New3Expression1right)) :: rest671)) => let val  
result = MlyValue.New2Expression (fn _ => let val  (New3Expression as 
New3Expression1) = New3Expression1 ()
 in ((New3Expression))
end)
 in ( LrTable.NT 24, ( result, New3Expression1left, 
New3Expression1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.New3Expression New3Expression1, _, 
New3Expression1right)) :: _ :: ( _, ( MlyValue.New2Expression 
New2Expression1, New2Expression1left, _)) :: rest671)) => let val  
result = MlyValue.New2Expression (fn _ => let val  (New2Expression as 
New2Expression1) = New2Expression1 ()
 val  (New3Expression as New3Expression1) = New3Expression1 ()
 in ((EQ(New2Expression, New3Expression)))
end)
 in ( LrTable.NT 24, ( result, New2Expression1left, 
New3Expression1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.New3Expression New3Expression1, _, 
New3Expression1right)) :: _ :: ( _, ( MlyValue.New2Expression 
New2Expression1, New2Expression1left, _)) :: rest671)) => let val  
result = MlyValue.New2Expression (fn _ => let val  (New2Expression as 
New2Expression1) = New2Expression1 ()
 val  (New3Expression as New3Expression1) = New3Expression1 ()
 in ((NE(New2Expression, New3Expression)))
end)
 in ( LrTable.NT 24, ( result, New2Expression1left, 
New3Expression1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.New4Expression New4Expression1, 
New4Expression1left, New4Expression1right)) :: rest671)) => let val  
result = MlyValue.New3Expression (fn _ => let val  (New4Expression as 
New4Expression1) = New4Expression1 ()
 in ((New4Expression))
end)
 in ( LrTable.NT 25, ( result, New4Expression1left, 
New4Expression1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.New4Expression New4Expression1, _, 
New4Expression1right)) :: _ :: ( _, ( MlyValue.New3Expression 
New3Expression1, New3Expression1left, _)) :: rest671)) => let val  
result = MlyValue.New3Expression (fn _ => let val  (New3Expression as 
New3Expression1) = New3Expression1 ()
 val  (New4Expression as New4Expression1) = New4Expression1 ()
 in ((GT(New3Expression, New4Expression)))
end)
 in ( LrTable.NT 25, ( result, New3Expression1left, 
New4Expression1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.New4Expression New4Expression1, _, 
New4Expression1right)) :: _ :: ( _, ( MlyValue.New3Expression 
New3Expression1, New3Expression1left, _)) :: rest671)) => let val  
result = MlyValue.New3Expression (fn _ => let val  (New3Expression as 
New3Expression1) = New3Expression1 ()
 val  (New4Expression as New4Expression1) = New4Expression1 ()
 in ((GE(New3Expression, New4Expression)))
end)
 in ( LrTable.NT 25, ( result, New3Expression1left, 
New4Expression1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.New4Expression New4Expression1, _, 
New4Expression1right)) :: _ :: ( _, ( MlyValue.New3Expression 
New3Expression1, New3Expression1left, _)) :: rest671)) => let val  
result = MlyValue.New3Expression (fn _ => let val  (New3Expression as 
New3Expression1) = New3Expression1 ()
 val  (New4Expression as New4Expression1) = New4Expression1 ()
 in ((LT(New3Expression, New4Expression)))
end)
 in ( LrTable.NT 25, ( result, New3Expression1left, 
New4Expression1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.New4Expression New4Expression1, _, 
New4Expression1right)) :: _ :: ( _, ( MlyValue.New3Expression 
New3Expression1, New3Expression1left, _)) :: rest671)) => let val  
result = MlyValue.New3Expression (fn _ => let val  (New3Expression as 
New3Expression1) = New3Expression1 ()
 val  (New4Expression as New4Expression1) = New4Expression1 ()
 in ((LE(New3Expression, New4Expression)))
end)
 in ( LrTable.NT 25, ( result, New3Expression1left, 
New4Expression1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.New5Expression New5Expression1, 
New5Expression1left, New5Expression1right)) :: rest671)) => let val  
result = MlyValue.New4Expression (fn _ => let val  (New5Expression as 
New5Expression1) = New5Expression1 ()
 in ((New5Expression))
end)
 in ( LrTable.NT 26, ( result, New5Expression1left, 
New5Expression1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.New5Expression New5Expression1, _, 
New5Expression1right)) :: _ :: ( _, ( MlyValue.New4Expression 
New4Expression1, New4Expression1left, _)) :: rest671)) => let val  
result = MlyValue.New4Expression (fn _ => let val  (New4Expression as 
New4Expression1) = New4Expression1 ()
 val  (New5Expression as New5Expression1) = New5Expression1 ()
 in ((PLUS(New4Expression, New5Expression)))
end)
 in ( LrTable.NT 26, ( result, New4Expression1left, 
New5Expression1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.New5Expression New5Expression1, _, 
New5Expression1right)) :: _ :: ( _, ( MlyValue.New4Expression 
New4Expression1, New4Expression1left, _)) :: rest671)) => let val  
result = MlyValue.New4Expression (fn _ => let val  (New4Expression as 
New4Expression1) = New4Expression1 ()
 val  (New5Expression as New5Expression1) = New5Expression1 ()
 in ((SUB(New4Expression, New5Expression)))
end)
 in ( LrTable.NT 26, ( result, New4Expression1left, 
New5Expression1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.New5Expression New5Expression1, _, 
New5Expression1right)) :: _ :: ( _, ( MlyValue.New4Expression 
New4Expression1, New4Expression1left, _)) :: rest671)) => let val  
result = MlyValue.New4Expression (fn _ => let val  (New4Expression as 
New4Expression1) = New4Expression1 ()
 val  (New5Expression as New5Expression1) = New5Expression1 ()
 in ((RATPLUS(New4Expression, New5Expression)))
end)
 in ( LrTable.NT 26, ( result, New4Expression1left, 
New5Expression1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.New5Expression New5Expression1, _, 
New5Expression1right)) :: _ :: ( _, ( MlyValue.New4Expression 
New4Expression1, New4Expression1left, _)) :: rest671)) => let val  
result = MlyValue.New4Expression (fn _ => let val  (New4Expression as 
New4Expression1) = New4Expression1 ()
 val  (New5Expression as New5Expression1) = New5Expression1 ()
 in ((RATSUB(New4Expression, New5Expression)))
end)
 in ( LrTable.NT 26, ( result, New4Expression1left, 
New5Expression1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.New6Expression New6Expression1, 
New6Expression1left, New6Expression1right)) :: rest671)) => let val  
result = MlyValue.New5Expression (fn _ => let val  (New6Expression as 
New6Expression1) = New6Expression1 ()
 in ((New6Expression))
end)
 in ( LrTable.NT 27, ( result, New6Expression1left, 
New6Expression1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.New6Expression New6Expression1, _, 
New6Expression1right)) :: _ :: ( _, ( MlyValue.New5Expression 
New5Expression1, New5Expression1left, _)) :: rest671)) => let val  
result = MlyValue.New5Expression (fn _ => let val  (New5Expression as 
New5Expression1) = New5Expression1 ()
 val  (New6Expression as New6Expression1) = New6Expression1 ()
 in ((TIMES(New5Expression, New6Expression)))
end)
 in ( LrTable.NT 27, ( result, New5Expression1left, 
New6Expression1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.New6Expression New6Expression1, _, 
New6Expression1right)) :: _ :: ( _, ( MlyValue.New5Expression 
New5Expression1, New5Expression1left, _)) :: rest671)) => let val  
result = MlyValue.New5Expression (fn _ => let val  (New5Expression as 
New5Expression1) = New5Expression1 ()
 val  (New6Expression as New6Expression1) = New6Expression1 ()
 in ((DIV(New5Expression, New6Expression)))
end)
 in ( LrTable.NT 27, ( result, New5Expression1left, 
New6Expression1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.New6Expression New6Expression1, _, 
New6Expression1right)) :: _ :: ( _, ( MlyValue.New5Expression 
New5Expression1, New5Expression1left, _)) :: rest671)) => let val  
result = MlyValue.New5Expression (fn _ => let val  (New5Expression as 
New5Expression1) = New5Expression1 ()
 val  (New6Expression as New6Expression1) = New6Expression1 ()
 in ((MOD(New5Expression, New6Expression)))
end)
 in ( LrTable.NT 27, ( result, New5Expression1left, 
New6Expression1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.New6Expression New6Expression1, _, 
New6Expression1right)) :: _ :: ( _, ( MlyValue.New5Expression 
New5Expression1, New5Expression1left, _)) :: rest671)) => let val  
result = MlyValue.New5Expression (fn _ => let val  (New5Expression as 
New5Expression1) = New5Expression1 ()
 val  (New6Expression as New6Expression1) = New6Expression1 ()
 in ((RATTIMES(New5Expression, New6Expression)))
end)
 in ( LrTable.NT 27, ( result, New5Expression1left, 
New6Expression1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.New6Expression New6Expression1, _, 
New6Expression1right)) :: _ :: ( _, ( MlyValue.New5Expression 
New5Expression1, New5Expression1left, _)) :: rest671)) => let val  
result = MlyValue.New5Expression (fn _ => let val  (New5Expression as 
New5Expression1) = New5Expression1 ()
 val  (New6Expression as New6Expression1) = New6Expression1 ()
 in ((RATDIV(New5Expression, New6Expression)))
end)
 in ( LrTable.NT 27, ( result, New5Expression1left, 
New6Expression1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.New7Expression New7Expression1, 
New7Expression1left, New7Expression1right)) :: rest671)) => let val  
result = MlyValue.New6Expression (fn _ => let val  (New7Expression as 
New7Expression1) = New7Expression1 ()
 in ((New7Expression))
end)
 in ( LrTable.NT 28, ( result, New7Expression1left, 
New7Expression1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.New7Expression New7Expression1, _, 
New7Expression1right)) :: ( _, ( _, INVERSE1left, _)) :: rest671)) =>
 let val  result = MlyValue.New6Expression (fn _ => let val  (
New7Expression as New7Expression1) = New7Expression1 ()
 in ((INVERSE(New7Expression)))
end)
 in ( LrTable.NT 28, ( result, INVERSE1left, New7Expression1right), 
rest671)
end
|  ( 47, ( ( _, ( MlyValue.New7Expression New7Expression1, _, 
New7Expression1right)) :: ( _, ( _, RAT1left, _)) :: rest671)) => let
 val  result = MlyValue.New6Expression (fn _ => let val  (
New7Expression as New7Expression1) = New7Expression1 ()
 in ((RAT(New7Expression)))
end)
 in ( LrTable.NT 28, ( result, RAT1left, New7Expression1right), 
rest671)
end
|  ( 48, ( ( _, ( MlyValue.New7Expression New7Expression1, _, 
New7Expression1right)) :: ( _, ( _, SHOWRAT1left, _)) :: rest671)) =>
 let val  result = MlyValue.New6Expression (fn _ => let val  (
New7Expression as New7Expression1) = New7Expression1 ()
 in ((SHOWRAT(New7Expression)))
end)
 in ( LrTable.NT 28, ( result, SHOWRAT1left, New7Expression1right), 
rest671)
end
|  ( 49, ( ( _, ( MlyValue.New7Expression New7Expression1, _, 
New7Expression1right)) :: ( _, ( _, SHOWDECIMAL1left, _)) :: rest671))
 => let val  result = MlyValue.New6Expression (fn _ => let val  (
New7Expression as New7Expression1) = New7Expression1 ()
 in ((SHOWDECIMAL(New7Expression)))
end)
 in ( LrTable.NT 28, ( result, SHOWDECIMAL1left, New7Expression1right)
, rest671)
end
|  ( 50, ( ( _, ( MlyValue.New7Expression New7Expression1, _, 
New7Expression1right)) :: ( _, ( _, FROMDECIMAL1left, _)) :: rest671))
 => let val  result = MlyValue.New6Expression (fn _ => let val  (
New7Expression as New7Expression1) = New7Expression1 ()
 in ((FROMDECIMAL(New7Expression)))
end)
 in ( LrTable.NT 28, ( result, FROMDECIMAL1left, New7Expression1right)
, rest671)
end
|  ( 51, ( ( _, ( MlyValue.New7Expression New7Expression1, _, 
New7Expression1right)) :: ( _, ( _, TODECIMAL1left, _)) :: rest671))
 => let val  result = MlyValue.New6Expression (fn _ => let val  (
New7Expression as New7Expression1) = New7Expression1 ()
 in ((TODECIMAL(New7Expression)))
end)
 in ( LrTable.NT 28, ( result, TODECIMAL1left, New7Expression1right), 
rest671)
end
|  ( 52, ( ( _, ( MlyValue.New7Expression New7Expression1, _, 
New7Expression1right)) :: ( _, ( _, NEG1left, _)) :: rest671)) => let
 val  result = MlyValue.New6Expression (fn _ => let val  (
New7Expression as New7Expression1) = New7Expression1 ()
 in ((NEG(New7Expression)))
end)
 in ( LrTable.NT 28, ( result, NEG1left, New7Expression1right), 
rest671)
end
|  ( 53, ( ( _, ( MlyValue.New7Expression New7Expression1, _, 
New7Expression1right)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let
 val  result = MlyValue.New6Expression (fn _ => let val  (
New7Expression as New7Expression1) = New7Expression1 ()
 in ((NOT(New7Expression)))
end)
 in ( LrTable.NT 28, ( result, NOT1left, New7Expression1right), 
rest671)
end
|  ( 54, ( ( _, ( MlyValue.NUMRAT NUMRAT1, NUMRAT1left, NUMRAT1right))
 :: rest671)) => let val  result = MlyValue.New7Expression (fn _ =>
 let val  (NUMRAT as NUMRAT1) = NUMRAT1 ()
 in ((RATIONAL_VALUE(NUMRAT)))
end)
 in ( LrTable.NT 29, ( result, NUMRAT1left, NUMRAT1right), rest671)

end
|  ( 55, ( ( _, ( MlyValue.NUMINT NUMINT1, NUMINT1left, NUMINT1right))
 :: rest671)) => let val  result = MlyValue.New7Expression (fn _ =>
 let val  (NUMINT as NUMINT1) = NUMINT1 ()
 in ((INTEGER_VALUE(NUMINT)))
end)
 in ( LrTable.NT 29, ( result, NUMINT1left, NUMINT1right), rest671)

end
|  ( 56, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.New7Expression (fn _ => ((TRUE)))
 in ( LrTable.NT 29, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 57, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.New7Expression (fn _ => ((FALSE)))
 in ( LrTable.NT 29, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 58, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.Expression 
Expression1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.New7Expression (fn _ => let val  (Expression
 as Expression1) = Expression1 ()
 in ((Expression))
end)
 in ( LrTable.NT 29, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 59, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.New7Expression (fn _ => let val  (ID as 
ID1) = ID1 ()
 in ((IDENTIFIER(ID)))
end)
 in ( LrTable.NT 29, ( result, ID1left, ID1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Calc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUMRAT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUMRAT (fn () => i),p1,p2))
fun NUMINT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.NUMINT (fn () => i),p1,p2))
fun RATIONALDEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun INTEGERDEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOLEANDEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun VARDEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun OD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun PROCEDURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun CALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun INVERSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun MAKERAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun SHOWRAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun SHOWDECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun FROMDECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun TODECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun LCOMMENT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun RCOMMENT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun RATPLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun RATSUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun RATTIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun RATDIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun NE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 51,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 52,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 53,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 54,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 55,(
ParserData.MlyValue.VOID,p1,p2))
end
end
