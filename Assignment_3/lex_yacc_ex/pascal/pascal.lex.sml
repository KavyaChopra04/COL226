functor PascalLexFun(structure Tokens : Pascal_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
B | C | INITIAL
    structure UserDeclarations = 
      struct

structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

open Tokens

val lineNum = ref 0
val eof = fn () => EOF(!lineNum,!lineNum)


structure KeyWord : sig
	     		val find : string ->
				 (int * int -> (svalue,int) token) option
	  	    end =
  struct

	val TableSize = 211
	val HashFactor = 5

	val hash = fn s =>
	   foldl (fn (c,v)=>(v*HashFactor+(ord c)) mod TableSize) 0 (explode s)


	val HashTable = Array.array(TableSize,nil) :
		 (string * (int * int -> (svalue,int) token)) list Array.array


	val add = fn (s,v) =>
	 let val i = hash s
	 in Array.update(HashTable,i,(s,v) :: (Array.sub(HashTable, i)))
	 end

        val find = fn s =>
	  let val i = hash s
	      fun f ((key,v)::r) = if s=key then SOME v else f r
	        | f nil = NONE
	  in  f (Array.sub(HashTable, i))
	  end
 
	val _ = 
	    (List.app add
	[("and",YAND),
	 ("array",YARRAY),
	 ("begin",YBEGIN),
	 ("case",YCASE),
	 ("const",YCONST),
	 ("div",YDIV),
	 ("do",YDO),
	 ("downto",YDOWNTO),
	 ("else",YELSE),
	 ("end",YEND),
	 ("extern",YEXTERN),
	 ("file",YFILE),
	 ("for",YFOR),
	 ("forward",YFORWARD),
	 ("function",YFUNCTION),
	 ("goto",YGOTO),
	 ("hex",YHEX),
	 ("if",YIF),
	 ("in",YIN),
	 ("label",YLABEL),
	 ("mod",YMOD),
	 ("nil",YNIL),
	 ("not",YNOT),
	 ("oct",YOCT),
	 ("of",YOF),
	 ("or",YOR),
	 ("packed",YPACKED),
	 ("procedure",YPROCEDURE),
	 ("program",YPROG),
	 ("record",YRECORD),
	 ("repeat",YREPEAT),
	 ("set",YSET),
	 ("then",YTHEN),
	 ("to",YTO),
	 ("type",YTYPE),
	 ("until",YUNTIL),
	 ("var",YVAR),
	 ("while",YWHILE),
	 ("with",YWITH)
	])
   end
   open KeyWord



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction1 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (lineNum := (!lineNum) + (String.size yytext); lex())
      end
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (case find yytext of SOME v => v(!lineNum,!lineNum)
						  | _ => YID(!lineNum,!lineNum))
      end
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YID(!lineNum,!lineNum)))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YNUMB(!lineNum,!lineNum)))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YINT(!lineNum,!lineNum)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YBINT(!lineNum,!lineNum)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YSTRING(!lineNum,!lineNum)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN C; lex()))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YDOTDOT(!lineNum,!lineNum)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YDOT(!lineNum,!lineNum)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YLPAR(!lineNum,!lineNum)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YRPAR(!lineNum,!lineNum)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YSEMI(!lineNum,!lineNum)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YCOMMA(!lineNum,!lineNum)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YCOLON(!lineNum,!lineNum)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YCARET(!lineNum,!lineNum)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YLBRA(!lineNum,!lineNum)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YRBRA(!lineNum,!lineNum)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YTILDE(!lineNum,!lineNum)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YLESS(!lineNum,!lineNum)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YEQUAL(!lineNum,!lineNum)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YGREATER(!lineNum,!lineNum)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YPLUS(!lineNum,!lineNum)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YMINUS(!lineNum,!lineNum)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YBAR(!lineNum,!lineNum)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YSTAR(!lineNum,!lineNum)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YSLASH(!lineNum,!lineNum)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN B; lex()))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YILLCH(!lineNum,!lineNum)))
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (lineNum := (!lineNum) + (String.size yytext); lex())
      end
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; lex()))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction35 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (lineNum := (!lineNum) + (String.size yytext); lex())
      end
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; lex()))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ40(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ40(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction3(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ40(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ40(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ41(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ40(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ40(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ41(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ41(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ40(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ40(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ41(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ41(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ41(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ46(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"0"
              then yyAction4(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ46(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ46(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ46(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yyQ45(strm', lastMatch)
            else if inp < #"-"
              then if inp = #"+"
                  then yyQ45(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"0"
              then yyQ46(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ46(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ44(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"E"
              then if inp = #"0"
                  then yyQ47(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ47(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ44(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ47(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ47(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"/"
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ42(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                      else yyAction5(strm, yyNO_MATCH)
                  else yyQ43(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp = #"F"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"F"
              then if inp = #"E"
                  then yyQ44(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ44(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"/"
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ42(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                      else yyAction5(strm, yyNO_MATCH)
                  else yyQ43(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp = #"F"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"F"
              then if inp = #"E"
                  then yyQ44(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ44(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"C"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"C"
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"."
                      then yyQ42(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                      else yyAction5(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"7"
                      then yyQ48(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                      else yyQ43(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp = #"B"
                  then yyQ49(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ49(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"E"
                  then yyQ44(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ44(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"C"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"C"
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"."
                      then yyQ42(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                      else yyAction5(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"7"
                      then yyQ48(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                      else yyQ43(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp = #"B"
                  then yyQ49(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ49(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"E"
                  then yyQ44(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ44(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ50(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ43(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #"0"
              then yyAction24(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ43(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ43(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"0"
              then yyAction23(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ43(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ51(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"'"
              then yyQ52(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
and yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"'"
              then yyQ53(strm', lastMatch)
              else yyQ52(strm', lastMatch)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"'"
              then yyQ53(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyQ52(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ16(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ54(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ54(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ54(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ54(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyQ28(strm', lastMatch)
            else if inp < #":"
              then if inp = #")"
                  then yyQ19(strm', lastMatch)
                else if inp < #")"
                  then if inp = #" "
                      then yyQ15(strm', lastMatch)
                    else if inp < #" "
                      then if inp = #"\n"
                          then yyQ16(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ15(strm', lastMatch)
                              else yyQ14(strm', lastMatch)
                          else yyQ14(strm', lastMatch)
                    else if inp = #"'"
                      then yyQ17(strm', lastMatch)
                    else if inp = #"("
                      then yyQ18(strm', lastMatch)
                      else yyQ14(strm', lastMatch)
                else if inp = #"."
                  then yyQ24(strm', lastMatch)
                else if inp < #"."
                  then if inp = #","
                      then yyQ22(strm', lastMatch)
                    else if inp < #","
                      then if inp = #"*"
                          then yyQ20(strm', lastMatch)
                          else yyQ21(strm', lastMatch)
                      else yyQ23(strm', lastMatch)
                else if inp = #"0"
                  then yyQ26(strm', lastMatch)
                else if inp < #"0"
                  then yyQ25(strm', lastMatch)
                else if inp <= #"7"
                  then yyQ26(strm', lastMatch)
                  else yyQ27(strm', lastMatch)
            else if inp = #"]"
              then yyQ35(strm', lastMatch)
            else if inp < #"]"
              then if inp = #"?"
                  then yyQ14(strm', lastMatch)
                else if inp < #"?"
                  then if inp = #"="
                      then yyQ31(strm', lastMatch)
                    else if inp < #"="
                      then if inp = #";"
                          then yyQ29(strm', lastMatch)
                          else yyQ30(strm', lastMatch)
                      else yyQ32(strm', lastMatch)
                else if inp = #"["
                  then yyQ34(strm', lastMatch)
                else if inp < #"["
                  then if inp <= #"@"
                      then yyQ14(strm', lastMatch)
                      else yyQ33(strm', lastMatch)
                  else yyQ14(strm', lastMatch)
            else if inp = #"|"
              then yyQ38(strm', lastMatch)
            else if inp < #"|"
              then if inp = #"a"
                  then yyQ33(strm', lastMatch)
                else if inp < #"a"
                  then if inp = #"^"
                      then yyQ36(strm', lastMatch)
                      else yyQ14(strm', lastMatch)
                else if inp = #"{"
                  then yyQ37(strm', lastMatch)
                  else yyQ33(strm', lastMatch)
            else if inp = #"~"
              then yyQ39(strm', lastMatch)
              else yyQ14(strm', lastMatch)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ12(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ13(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ8(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ7(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ7(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = #"("
              then yyAction31(strm, yyNO_MATCH)
            else if inp < #"("
              then yyQ7(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= #"*"
              then yyAction31(strm, yyNO_MATCH)
              else yyQ7(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"("
              then yyQ9(strm', lastMatch)
            else if inp < #"("
              then if inp = #"\n"
                  then yyQ8(strm', lastMatch)
                  else yyQ7(strm', lastMatch)
            else if inp = #"*"
              then yyQ11(strm', lastMatch)
            else if inp = #")"
              then yyQ10(strm', lastMatch)
              else yyQ7(strm', lastMatch)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ4(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"{"
              then yyAction36(strm, yyNO_MATCH)
            else if inp < #"{"
              then if inp = #"\n"
                  then yyAction36(strm, yyNO_MATCH)
                  else yyQ3(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp = #"}"
              then yyAction36(strm, yyNO_MATCH)
              else yyQ3(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"{"
              then yyQ5(strm', lastMatch)
            else if inp < #"{"
              then if inp = #"\n"
                  then yyQ4(strm', lastMatch)
                  else yyQ3(strm', lastMatch)
            else if inp = #"}"
              then yyQ6(strm', lastMatch)
              else yyQ3(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of B => yyQ0(!(yystrm), yyNO_MATCH)
    | C => yyQ1(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ2(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
