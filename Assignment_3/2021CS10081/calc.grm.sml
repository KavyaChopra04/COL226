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
val ListofBindings: (string * ExpOp.rational) list ref = ref [];
exception UndeclaredBinding
fun intlistToString []      = ""
  | intlistToString [(x,y)]     = x^" : "^ExpOp.showDecimal(y)
  | intlistToString ((x,y)::xs) = x^" : "^ExpOp.showDecimal(y)^ " , " ^ intlistToString xs 
fun dropWhile(x) = 
  if(null(x)) then x
  else if(hd(x) = #" ") then dropWhile(tl(x))
  else x
fun trimsp(s) = 
  let 
    val x = rev(explode(s))
    val y = dropWhile(x)
    val z = implode(rev(y))
  in 
    z
  end

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
\\001\000\001\000\009\000\002\000\008\000\010\000\007\000\000\000\
\\001\000\001\000\015\000\000\000\
\\001\000\001\000\025\000\000\000\
\\001\000\003\000\013\000\009\000\012\000\011\000\024\000\000\000\
\\001\000\006\000\000\000\007\000\000\000\000\000\
\\001\000\013\000\022\000\014\000\021\000\000\000\
\\001\000\016\000\023\000\000\000\
\\001\000\016\000\028\000\000\000\
\\031\000\001\000\009\000\002\000\008\000\010\000\007\000\012\000\006\000\000\000\
\\032\000\001\000\009\000\002\000\008\000\010\000\007\000\000\000\
\\033\000\003\000\013\000\009\000\012\000\000\000\
\\034\000\003\000\013\000\009\000\012\000\000\000\
\\035\000\003\000\013\000\009\000\012\000\000\000\
\\036\000\003\000\013\000\009\000\012\000\000\000\
\\037\000\004\000\011\000\008\000\010\000\000\000\
\\038\000\004\000\011\000\008\000\010\000\000\000\
\\039\000\004\000\011\000\008\000\010\000\000\000\
\\040\000\000\000\
\\041\000\000\000\
\\042\000\000\000\
\\043\000\000\000\
\\044\000\000\000\
\\045\000\000\000\
\"
val actionRowNumbers =
"\008\000\017\000\014\000\011\000\
\\001\000\000\000\020\000\022\000\
\\000\000\000\000\000\000\000\000\
\\005\000\006\000\003\000\019\000\
\\018\000\016\000\015\000\002\000\
\\009\000\000\000\021\000\007\000\
\\010\000\013\000\000\000\012\000\
\\004\000"
val gotoT =
"\
\\001\000\003\000\003\000\002\000\005\000\001\000\007\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\012\000\000\000\
\\001\000\014\000\003\000\002\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\005\000\015\000\000\000\
\\005\000\016\000\000\000\
\\003\000\017\000\005\000\001\000\000\000\
\\003\000\018\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\024\000\003\000\002\000\005\000\001\000\000\000\
\\001\000\025\000\003\000\002\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\027\000\003\000\002\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 29
val numrules = 15
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
 | NUM of unit ->  (ExpOp.rational) | ID of unit ->  (string)
 | BINDING of unit ->  ( ( string * ExpOp.rational )  list ref)
 | START of unit ->  ( ( (string * ExpOp.rational) list ref * ExpOp.rational option  ) )
 | FACTOR1 of unit ->  (ExpOp.rational)
 | FACTOR of unit ->  (ExpOp.rational)
 | TERM1 of unit ->  (ExpOp.rational)
 | TERM of unit ->  (ExpOp.rational)
 | EXP1 of unit ->  (ExpOp.rational)
 | EXP of unit ->  (ExpOp.rational)
end
type svalue = MlyValue.svalue
type result = 
 ( (string * ExpOp.rational) list ref * ExpOp.rational option  ) 
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 4) => true | (T 5) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
 $$ (T 0),nil
 $$ (T 4))::
nil
val noShift = 
fn (T 6) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "NUM"
  | (T 2) => "PLUS"
  | (T 3) => "TIMES"
  | (T 4) => "PRINT"
  | (T 5) => "SEMI"
  | (T 6) => "EOF"
  | (T 7) => "DIV"
  | (T 8) => "SUB"
  | (T 9) => "LPAREN"
  | (T 10) => "RPAREN"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "COMMA"
  | (T 14) => "ASSGN"
  | (T 15) => "EQ"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( rest671)) => let val  result = MlyValue.START (fn _ => (
ref [], NONE))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 1, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.BINDING 
BINDING1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.START (fn _ => let val  (BINDING as BINDING1)
 = BINDING1 ()
 in (BINDING, NONE)
end)
 in ( LrTable.NT 6, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.BINDING BINDING1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: 
rest671)) => let val  result = MlyValue.START (fn _ => let val  (
BINDING as BINDING1) = BINDING1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (BINDING, SOME EXP)
end)
 in ( LrTable.NT 6, ( result, LBRACE1left, EXP1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671))
 => let val  result = MlyValue.START (fn _ => let val  (EXP as EXP1) =
 EXP1 ()
 in (ref [], SOME EXP)
end)
 in ( LrTable.NT 6, ( result, EXP1left, EXP1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: _ :: ( _, ( MlyValue.BINDING BINDING1, 
BINDING1left, _)) :: rest671)) => let val  result = MlyValue.BINDING
 (fn _ => let val  BINDING1 = BINDING1 ()
 val  (ID as ID1) = ID1 ()
 val  EXP1 = EXP1 ()
 in ((addBinding(ID, EXP1, ListofBindings)))
end)
 in ( LrTable.NT 7, ( result, BINDING1left, EXP1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.BINDING (fn _ => let val  (ID as ID1) = ID1 ()
 val  EXP1 = EXP1 ()
 in ((addBinding(ID, EXP1, ListofBindings)))
end)
 in ( LrTable.NT 7, ( result, ID1left, EXP1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.TERM TERM1, TERM1left, TERM1right)) :: 
rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (TERM
 as TERM1) = TERM1 ()
 in (TERM)
end)
 in ( LrTable.NT 0, ( result, TERM1left, TERM1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.TERM TERM1, _, TERM1right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  (TERM as TERM1) = TERM1 ()
 in (ExpOp.add(EXP,TERM))
end)
 in ( LrTable.NT 0, ( result, EXP1left, TERM1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.TERM TERM1, _, TERM1right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  (TERM as TERM1) = TERM1 ()
 in (ExpOp.subtract(EXP,TERM))
end)
 in ( LrTable.NT 0, ( result, EXP1left, TERM1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.FACTOR FACTOR1, FACTOR1left, FACTOR1right))
 :: rest671)) => let val  result = MlyValue.TERM (fn _ => let val  (
FACTOR as FACTOR1) = FACTOR1 ()
 in (FACTOR)
end)
 in ( LrTable.NT 2, ( result, FACTOR1left, FACTOR1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.FACTOR FACTOR1, _, FACTOR1right)) :: _ :: (
 _, ( MlyValue.TERM TERM1, TERM1left, _)) :: rest671)) => let val  
result = MlyValue.TERM (fn _ => let val  (TERM as TERM1) = TERM1 ()
 val  (FACTOR as FACTOR1) = FACTOR1 ()
 in (ExpOp.multiply(TERM,FACTOR))
end)
 in ( LrTable.NT 2, ( result, TERM1left, FACTOR1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.FACTOR FACTOR1, _, FACTOR1right)) :: _ :: (
 _, ( MlyValue.TERM TERM1, TERM1left, _)) :: rest671)) => let val  
result = MlyValue.TERM (fn _ => let val  (TERM as TERM1) = TERM1 ()
 val  (FACTOR as FACTOR1) = FACTOR1 ()
 in (valOf(ExpOp.divide(TERM, FACTOR)))
end)
 in ( LrTable.NT 2, ( result, TERM1left, FACTOR1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.FACTOR (fn _ => let val  (NUM as NUM1)
 = NUM1 ()
 in (NUM)
end)
 in ( LrTable.NT 4, ( result, NUM1left, NUM1right), rest671)
end
|  ( 13, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.FACTOR (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (EXP)
end)
 in ( LrTable.NT 4, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.FACTOR (fn _ => let val  (ID as ID1) = ID1
 ()
 in ((finserval((trimsp(ID)), !ListofBindings)))
end)
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
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
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
end
end
