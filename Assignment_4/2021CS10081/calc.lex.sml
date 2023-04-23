functor CalcLexFun(structure Tokens: Calc_TOKENS)  = struct

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
INITIAL
    structure UserDeclarations = 
      struct

structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token
val pos = ref 0
fun eof () = Tokens.EOF(!pos,!pos)


      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
#[([(#"\t",#"\t",1),
(#" ",#" ",1),
(#"\n",#"\n",2),
(#"!",#"!",3),
(#"%",#"%",4),
(#"&",#"&",5),
(#"(",#"(",6),
(#")",#")",7),
(#"*",#"*",8),
(#"+",#"+",9),
(#",",#",",10),
(#"-",#"-",11),
(#".",#".",12),
(#"/",#"/",13),
(#"0",#"9",14),
(#":",#":",15),
(#";",#";",16),
(#"<",#"<",17),
(#"=",#"=",18),
(#">",#">",19),
(#"A",#"Z",20),
(#"a",#"a",20),
(#"g",#"h",20),
(#"j",#"l",20),
(#"n",#"n",20),
(#"q",#"q",20),
(#"u",#"u",20),
(#"x",#"z",20),
(#"b",#"b",21),
(#"c",#"c",22),
(#"d",#"d",23),
(#"e",#"e",24),
(#"f",#"f",25),
(#"i",#"i",26),
(#"m",#"m",27),
(#"o",#"o",28),
(#"p",#"p",29),
(#"r",#"r",30),
(#"s",#"s",31),
(#"t",#"t",32),
(#"v",#"v",33),
(#"w",#"w",34),
(#"{",#"{",35),
(#"|",#"|",36),
(#"}",#"}",37),
(#"~",#"~",38)], []), ([(#"\t",#"\t",1),
(#" ",#" ",1)], [1]), ([], [0]), ([], [29]), ([], [44]), ([(#"&",#"&",158)], []), ([(#"*",#"*",154)], [52]), ([], [53]), ([(#")",#")",153)], [42]), ([], [40]), ([], [57]), ([], [41]), ([(#"(",#"(",142),
(#"*",#"*",145),
(#"+",#"+",146),
(#"-",#"-",147),
(#"/",#"/",148),
(#"0",#"9",141)], []), ([], [43]), ([(#".",#".",141),
(#"0",#"9",14)], [4]), ([(#"=",#"=",140)], []), ([], [56]), ([(#"=",#"=",138),
(#">",#">",139)], [45]), ([], [49]), ([(#"=",#"=",137)], [47]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"n",20),
(#"p",#"z",20),
(#"o",#"o",131)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",128)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"n",20),
(#"p",#"z",20),
(#"o",#"o",127)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",124)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"e",20),
(#"g",#"h",20),
(#"j",#"q",20),
(#"s",#"z",20),
(#"f",#"f",112),
(#"i",#"i",113),
(#"r",#"r",114)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"e",20),
(#"g",#"m",20),
(#"o",#"z",20),
(#"f",#"f",100),
(#"n",#"n",101)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",93)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"c",20),
(#"e",#"z",20),
(#"d",#"d",92)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"q",20),
(#"s",#"z",20),
(#"r",#"r",81)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"d",20),
(#"f",#"z",20),
(#"a",#"a",71),
(#"e",#"e",72)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"g",20),
(#"i",#"z",20),
(#"h",#"h",58)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"g",20),
(#"i",#"n",20),
(#"p",#"s",20),
(#"u",#"z",20),
(#"h",#"h",46),
(#"o",#"o",47),
(#"t",#"t",48)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",44)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"g",20),
(#"i",#"z",20),
(#"h",#"h",40)], [32]), ([], [54]), ([(#"|",#"|",39)], []), ([], [55]), ([], [39]), ([], [31]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"h",20),
(#"j",#"z",20),
(#"i",#"i",41)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",42)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",43)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [15, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"q",20),
(#"s",#"z",20),
(#"r",#"r",45)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [10, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",56)], [32]), ([(#"0",#"9",20),
(#"A",#"C",20),
(#"E",#"Z",20),
(#"a",#"z",20),
(#"D",#"D",49)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [8, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",50)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"b",20),
(#"d",#"z",20),
(#"c",#"c",51)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"h",20),
(#"j",#"z",20),
(#"i",#"i",52)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"l",20),
(#"n",#"z",20),
(#"m",#"m",53)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",54)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",55)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [28, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"m",20),
(#"o",#"z",20),
(#"n",#"n",57)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [12, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"n",20),
(#"p",#"z",20),
(#"o",#"o",59)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"v",20),
(#"x",#"z",20),
(#"w",#"w",60)], [32]), ([(#"0",#"9",20),
(#"A",#"C",20),
(#"E",#"Q",20),
(#"S",#"Z",20),
(#"a",#"z",20),
(#"D",#"D",61),
(#"R",#"R",62)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",65)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",63)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"s",20),
(#"u",#"z",20),
(#"t",#"t",64)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [25, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"b",20),
(#"d",#"z",20),
(#"c",#"c",66)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"h",20),
(#"j",#"z",20),
(#"i",#"i",67)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"l",20),
(#"n",#"z",20),
(#"m",#"m",68)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",69)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",70)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [26, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"s",20),
(#"u",#"z",20),
(#"t",#"t",75)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",73)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"c",20),
(#"e",#"z",20),
(#"d",#"d",74)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [20, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"h",20),
(#"j",#"z",20),
(#"i",#"i",76)], [24, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"n",20),
(#"p",#"z",20),
(#"o",#"o",77)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"m",20),
(#"o",#"z",20),
(#"n",#"n",78)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",79)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",80)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [5, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"h",20),
(#"j",#"n",20),
(#"p",#"z",20),
(#"i",#"i",82),
(#"o",#"o",83)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"m",20),
(#"o",#"z",20),
(#"n",#"n",90)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"b",20),
(#"d",#"z",20),
(#"c",#"c",84)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",85)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"c",20),
(#"e",#"z",20),
(#"d",#"d",86)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"t",20),
(#"v",#"z",20),
(#"u",#"u",87)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"q",20),
(#"s",#"z",20),
(#"r",#"r",88)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",89)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [18, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"s",20),
(#"u",#"z",20),
(#"t",#"t",91)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [19, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [17, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"j",20),
(#"l",#"z",20),
(#"k",#"k",94)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",95)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20),
(#"_",#"_",96)], [32]), ([(#"r",#"r",97)], []), ([(#"a",#"a",98)], []), ([(#"t",#"t",99)], []), ([], [23]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [11, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"s",20),
(#"u",#"u",20),
(#"w",#"z",20),
(#"t",#"t",102),
(#"v",#"v",103)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",108)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",104)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"q",20),
(#"s",#"z",20),
(#"r",#"r",105)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"r",20),
(#"t",#"z",20),
(#"s",#"s",106)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",107)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [22, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"f",20),
(#"h",#"z",20),
(#"g",#"g",109)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",110)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"q",20),
(#"s",#"z",20),
(#"r",#"r",111)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [6, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [9, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [14, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"n",20),
(#"p",#"z",20),
(#"o",#"o",115)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"l",20),
(#"n",#"z",20),
(#"m",#"m",116)], [32]), ([(#"0",#"9",20),
(#"A",#"C",20),
(#"E",#"Z",20),
(#"a",#"z",20),
(#"D",#"D",117)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",118)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"b",20),
(#"d",#"z",20),
(#"c",#"c",119)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"h",20),
(#"j",#"z",20),
(#"i",#"i",120)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"l",20),
(#"n",#"z",20),
(#"m",#"m",121)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",122)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",123)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [27, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"r",20),
(#"t",#"z",20),
(#"s",#"s",125)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",126)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [13, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [16, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",129)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",130)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [21, 32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"n",20),
(#"p",#"z",20),
(#"o",#"o",132)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",133)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",134)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",135)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"m",20),
(#"o",#"z",20),
(#"n",#"n",136)], [32]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [7, 32]), ([], [48]), ([], [46]), ([], [50]), ([], [51]), ([(#"(",#"(",142),
(#"0",#"9",141)], []), ([(#"0",#"9",143)], []), ([(#")",#")",144),
(#"0",#"9",143)], []), ([], [3]), ([(#".",#".",152)], []), ([(#".",#".",151)], []), ([(#".",#".",150)], []), ([(#".",#".",149)], []), ([], [38]), ([], [36]), ([], [35]), ([], [37]), ([], [34]), ([(#"\^@",#")",155),
(#"+",#"\255",155),
(#"*",#"*",156)], [33]), ([(#"\^@",#")",155),
(#"+",#"\255",155),
(#"*",#"*",156)], []), ([(#"\^@",#"(",155),
(#"+",#".",155),
(#"0",#"\255",155),
(#")",#")",157),
(#"*",#"*",156)], []), ([], [2]), ([], [30])]
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
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (pos := !pos + 1; lex()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction3 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.NUMRAT((ExpOp.fromDecimal(yytext)), !pos, !pos))
      end
fun yyAction4 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.NUMINT ((BigInt.toBigint(yytext)), !pos, !pos))
      end
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RATIONALDEC(!pos,!pos)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INTEGERDEC(!pos,!pos)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BOOLEANDEC(!pos,!pos)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TRUE(!pos,!pos)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FALSE(!pos,!pos)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.VARDEC(!pos,!pos)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.IF(!pos,!pos)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.THEN(!pos,!pos)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ELSE(!pos,!pos)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FI(!pos,!pos)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.WHILE(!pos,!pos)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DO(!pos,!pos)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OD(!pos,!pos)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PROCEDURE(!pos,!pos)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PRINT(!pos,!pos)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.READ(!pos,!pos)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.CALL(!pos,!pos)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INVERSE(!pos,!pos)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MAKERAT(!pos,!pos)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RAT(!pos,!pos)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SHOWRAT(!pos,!pos)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SHOWDECIMAL(!pos,!pos)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FROMDECIMAL(!pos,!pos)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TODECIMAL(!pos,!pos)))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NOT(!pos,!pos)))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.AND(!pos,!pos)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OR(!pos,!pos)))
fun yyAction32 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ID(yytext,!pos,!pos))
      end
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LCOMMENT(!pos,!pos)))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RCOMMENT(!pos,!pos)))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RATPLUS(!pos,!pos)))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RATSUB(!pos,!pos)))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RATTIMES(!pos,!pos)))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RATDIV(!pos,!pos)))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NEG(!pos,!pos)))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PLUS(!pos,!pos)))
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SUB(!pos,!pos)))
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TIMES(!pos,!pos)))
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DIV(!pos,!pos)))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MOD(!pos,!pos)))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LT(!pos,!pos)))
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LE(!pos,!pos)))
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GT(!pos,!pos)))
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GE(!pos,!pos)))
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQ(!pos,!pos)))
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NE(!pos,!pos)))
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ASSIGN(!pos,!pos)))
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LPAREN(!pos,!pos)))
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPAREN(!pos,!pos)))
fun yyAction54 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACE(!pos,!pos)))
fun yyAction55 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACE(!pos,!pos)))
fun yyAction56 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMI(!pos,!pos)))
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(!pos,!pos)))
val yyactTable = Vector.fromList([yyAction0, yyAction1, yyAction2, yyAction3,
  yyAction4, yyAction5, yyAction6, yyAction7, yyAction8, yyAction9, yyAction10,
  yyAction11, yyAction12, yyAction13, yyAction14, yyAction15, yyAction16,
  yyAction17, yyAction18, yyAction19, yyAction20, yyAction21, yyAction22,
  yyAction23, yyAction24, yyAction25, yyAction26, yyAction27, yyAction28,
  yyAction29, yyAction30, yyAction31, yyAction32, yyAction33, yyAction34,
  yyAction35, yyAction36, yyAction37, yyAction38, yyAction39, yyAction40,
  yyAction41, yyAction42, yyAction43, yyAction44, yyAction45, yyAction46,
  yyAction47, yyAction48, yyAction49, yyAction50, yyAction51, yyAction52,
  yyAction53, yyAction54, yyAction55, yyAction56, yyAction57])
in
  if yyInput.eof(!(yystrm))
    then UserDeclarations.eof(yyarg)
    else (case (!(yyss))
       of INITIAL => yygo yyactTable (0, !(yystrm), yyNO_MATCH)
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
