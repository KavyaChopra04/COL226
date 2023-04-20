(* while_ast.sml *)
structure Calc :
sig val compile : string -> DataTypes.AST
end =
    struct
    exception CalcError;
    fun compile (fileName) =
    let
        val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn
        n => if TextIO.endOfStream inStream
            then ""
            else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
        (msg,line,col) =>
        print (fileName^"["^Int.toString line^":"
        ^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = CalcParser.parse
        (15,
        (CalcParser.makeLexer grab ),
        printError,
        ()) 
        handle CalcParser.ParseError => raise CalcError ; 
        (* Close the source program file *)
        val _ = TextIO.closeIn inStream;

    in 
        tree 
    end
end;