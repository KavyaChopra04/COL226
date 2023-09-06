(* while_ast.sml *)
structure Calc:
sig 
    exception UndeclaredBinding;
    exception UndeclaredProcedure;
    exception RedeclaredBinding; 
    exception TypeMismatch;
    exception InvalidBooleanExpressioninIf;
    exception InvalidBooleanExpressioninWhile;
    exception InvalidBooleanInput;
    (* val Calc_Tree : DataTypes.AST;
    val Calc_Block : DataTypes.BLOCK; *)
    datatype CALLFRAME = CALLFRAME of int * int * (string, DataTypes.EXP) HashTable.hash_table* (string, DataTypes.BLOCK) HashTable.hash_table;
    val callstack: CALLFRAME list ref
    val compile : string -> DataTypes.AST
    val retBlock : string -> DataTypes.BLOCK
    val parentTable: (string, int) HashTable.hash_table
    val globalScopedSymbolTable: (string, (string, DataTypes.TYPE) HashTable.hash_table) HashTable.hash_table
    val numberScopes : DataTypes.BLOCK * int ref -> unit
    val printParentTable: unit -> unit
    val printGlobalTable: unit -> unit
    val setZero: DataTypes.TYPE -> DataTypes.EXP
    val evaluateEXP: DataTypes.EXP * int-> DataTypes.EXP * DataTypes.TYPE

    val runProgram: DataTypes.BLOCK -> unit
    val interpret: string *string  -> unit
end =
    struct
    exception CalcError;
    exception UndeclaredBinding;
    exception RedeclaredBinding;
    exception InvalidBooleanExpressioninIf;
    exception InvalidBooleanExpressioninWhile;
    exception InvalidBooleanInput;
    exception TypeMismatch;
    exception UndeclaredProcedure;
    val inputFile:string ref = ref "input.rat";
    val outputFile:string ref= ref "output.rat";
    val callframe_id: int ref = ref 0;
    datatype CALLFRAME = CALLFRAME of int * int * (string, DataTypes.EXP) HashTable.hash_table * (string, DataTypes.BLOCK) HashTable.hash_table;
    val callstack: CALLFRAME list ref = ref [];
    val parentTable: (string, int) HashTable.hash_table = HashTable.mkTable (HashString.hashString, op=) (42, Fail "not found")
    val globalScopedSymbolTable: (string, (string, DataTypes.TYPE) HashTable.hash_table) HashTable.hash_table = HashTable.mkTable (HashString.hashString, op=) (42, Fail "not found")
    fun initFile()=
        let val fd = TextIO.openOut (!outputFile)
            val _ = TextIO.closeOut fd
        in () end
    fun writeFile content =
        let val fd = TextIO.openAppend (!outputFile)
            val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        in () end
    fun closeFile () =
        let val fd = TextIO.openAppend (!outputFile)
            val _ = TextIO.closeOut fd
        in () end
    fun setZero(DataTypes.INT) = DataTypes.INTEGER_VALUE(BigInt.repZero)
        | setZero(DataTypes.RATIONAL) = DataTypes.RATIONAL_VALUE(valOf(ExpOp.rat(BigInt.repZero)))
        | setZero(DataTypes.BOOL) = DataTypes.FALSE
        | setZero(DataTypes.PROCEDURE) = DataTypes.IDENTIFIER("")
        | setZero(DataTypes.STRING) = DataTypes.IDENTIFIER("")

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
    fun numberScopes (blk : DataTypes.BLOCK, x: int ref) =
        let
            val DataTypes.BLOCK(declist, procdeflist, cmdlist, scope_table, scopeindex) = blk;
            val DataTypes.SCOPE_TABLE(scopetable) = scope_table;
            val q = HashTable.insert globalScopedSymbolTable(Int.toString(!x), scopetable);
        in
            (scopeindex := !x;
            x := !x + 1;
            List.app (fn DataTypes.PROCDEF(name, block:DataTypes.BLOCK) =>  (let val p = HashTable.insert parentTable(Int.toString(!x), !scopeindex);  val k = numberScopes(block, x); 
            in x := !x  end)) procdeflist)
        end

    fun getType(x: DataTypes.EXP) = 
        case x of 
            DataTypes.INTEGER_VALUE(i) => DataTypes.INT
        |   DataTypes.TRUE => DataTypes.BOOL
        |   DataTypes.FALSE => DataTypes.BOOL
        |   DataTypes.IDENTIFIER(s) => DataTypes.STRING
        |   DataTypes.RATIONAL_VALUE(r) => DataTypes.RATIONAL
        |   DataTypes.STRING_VALUE(s) => DataTypes.STRING
    fun retBlock(s) =
        let
            val DataTypes.PROG(block) = compile s;
        in
            (numberScopes(block, ref 1);
            block)
        end
    fun intlistToStringnor []      = "fin"
        | intlistToStringnor ((x,y)::xs) = x^"'s parent is : "^Int.toString(y)^ " , " ^ intlistToStringnor xs 
    fun intlistToString []      = "fin"
        | intlistToString ((x,DataTypes.RATIONAL)::xs) = x^" : "^"RATIONAL"^ " , " ^ intlistToString xs 
        | intlistToString ((x,DataTypes.INT)::xs) = x^" : "^"INT"^ " , " ^ intlistToString xs
        | intlistToString ((x,DataTypes.BOOL)::xs) = x^" : "^"BOOLEAN"^ " , " ^ intlistToString xs
        | intlistToString ((x,DataTypes.PROCEDURE)::xs) = x^" : "^"PROCEDURE"^ " , " ^ intlistToString xs
    fun printParentTable ()= 
        let
            val x = HashTable.listItemsi parentTable;
        in
            (print(intlistToStringnor x))
        end
    fun printGlobalTable ()=
        let
            val x = HashTable.listItemsi globalScopedSymbolTable;
            val y = List.map (fn (a,b) => (a, HashTable.listItemsi b)) x;
        in
            (List.app (fn (a,b) => (print(a^" : "^intlistToString b))) y)
        end
    
    fun lookup(var, callstack, scopeindex) = 
        if(null (callstack))then 
            raise UndeclaredBinding
        else
            let
                val CALLFRAME(callframe_id, scope_index, currentFrame, proctable) = hd(callstack);
                val parentScope = HashTable.find parentTable (Int.toString(scopeindex))
            in
                if(isSome(HashTable.find currentFrame var)=false andalso scope_index=scopeindex) then
                    if(parentScope <> NONE) then
                        (* print("searching for variable" ^ var ^ " in parentscope "^Int.toString(valOf(parentScope))^"\n"); *)
                        lookup(var, tl(callstack), valOf(parentScope))
                    else
                        raise UndeclaredBinding
                else 
                    if (isSome(HashTable.find currentFrame var)=false  andalso scope_index<>scopeindex) then
                        (* print("searching procedure "^var^"further down, the current callframe has scope "^(Int.toString(scopeindex))^"\n");   *)
                        lookup(var, tl(callstack), scopeindex)
                    else 
                        if(isSome(HashTable.find currentFrame var) andalso scope_index = scopeindex) then
                            (* print("found value "^var^ "in current scope "^Int.toString(scopeindex)^"\n"); *)
                            valOf(HashTable.find currentFrame var)    
                        else    
                            (* print("searching for variable" ^ var ^" further down, the current callframe has scope "^(Int.toString(scope_index))^"\n"); *)
                            lookup(var, tl(callstack), scopeindex)         

            end    
    fun proclookup(var: string, callstack, scopeindex) = 
        if(null (callstack))then 
            raise UndeclaredProcedure
        else
            let
                val CALLFRAME(callframe_id, scope_index, currentFrame, proctable:(string, DataTypes.BLOCK) HashTable.hash_table ) = hd(callstack);
                val parentScope = HashTable.find parentTable (Int.toString(scopeindex))
            in
                if(isSome(HashTable.find proctable var)=false andalso scope_index=scopeindex) then
                    if(parentScope <> NONE) then
                        (* print("searching for procedure "^var^"in parentscope "^Int.toString(valOf(parentScope))^"\n"); *)
                        proclookup(var, tl(callstack), valOf(parentScope))
                    else
                        raise UndeclaredProcedure
                else 
                    if (isSome(HashTable.find proctable var)=false andalso scope_index<>scopeindex) then
                        (* (print("searching procedure "^var^"further down, the current callframe has scope "^(Int.toString(scopeindex))^"\n");                         *)
                        proclookup(var, tl(callstack), scopeindex)
                    else 
                        if(isSome(HashTable.find proctable var) andalso scope_index = scopeindex) then
                            (* (print("found procedure"^var^"in current scope "^Int.toString(scopeindex)^"\n"); *)
                           valOf(HashTable.find proctable var)     
                        else    
                            (* (print("searching for procedure" ^var^"further down, the current callframe has scope "^(Int.toString(scope_index))^"\n"); *)
                            proclookup(var, tl(callstack), scopeindex)    
            end
    fun evaluateEXP(exp: DataTypes.EXP, scopeindex) =
        let
            val k = 1;
        in
            case exp of DataTypes.INTEGER_VALUE(i) => (DataTypes.INTEGER_VALUE(i),DataTypes.INT)
            |   DataTypes.TRUE => (DataTypes.TRUE,DataTypes.BOOL)
            |   DataTypes.FALSE => (DataTypes.FALSE,DataTypes.BOOL)
            |   DataTypes.IDENTIFIER(s) => (lookup(s, !callstack, scopeindex), getType(lookup(s, !callstack, scopeindex)))
            |   DataTypes.RATIONAL_VALUE(r) => (DataTypes.RATIONAL_VALUE(r),DataTypes.RATIONAL)
            |   DataTypes.PLUS(e1,e2) =>    let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.INT andalso type2 = DataTypes.INT) 
                                                then
                                                    let
                                                        val DataTypes.INTEGER_VALUE(i1) = val1;
                                                        val DataTypes.INTEGER_VALUE(i2) = val2;
                                                    in 
                                                        (DataTypes.INTEGER_VALUE(BigInt.addBigint(i1, i2)),DataTypes.INT)
                                                    end
                                                else raise TypeMismatch
                                            end
            |   DataTypes.SUB(e1,e2) =>     let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.INT andalso type2 = DataTypes.INT) 
                                                then
                                                    let
                                                        val DataTypes.INTEGER_VALUE(i1) = val1;
                                                        val DataTypes.INTEGER_VALUE(i2) = val2;
                                                    in 
                                                        (DataTypes.INTEGER_VALUE(BigInt.subtractBigint(i1, i2)),DataTypes.INT)
                                                    end
                                                else raise TypeMismatch
                                            end
            
            |   DataTypes.TIMES(e1,e2) =>   let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.INT andalso type2 = DataTypes.INT) 
                                                then
                                                    let
                                                        val DataTypes.INTEGER_VALUE(i1) = val1;
                                                        val DataTypes.INTEGER_VALUE(i2) = val2;
                                                    in 
                                                        (DataTypes.INTEGER_VALUE(BigInt.multiplyBigint(i1, i2)),DataTypes.INT)
                                                    end
                                                else raise TypeMismatch
                                            end
            |   DataTypes.DIV(e1,e2) =>     let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.INT andalso type2 = DataTypes.INT) 
                                                then
                                                    let
                                                        val DataTypes.INTEGER_VALUE(i1) = val1;
                                                        val DataTypes.INTEGER_VALUE(i2) = val2;
                                                    in 
                                                        (DataTypes.INTEGER_VALUE(BigInt.divideBigint(i1, i2)),DataTypes.INT)
                                                    end
                                                else raise TypeMismatch
                                            end
            |   DataTypes.MOD(e1,e2) =>     let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.INT andalso type2 = DataTypes.INT) 
                                                then
                                                    let
                                                        val DataTypes.INTEGER_VALUE(i1) = val1;
                                                        val DataTypes.INTEGER_VALUE(i2) = val2;
                                                    in 
                                                        (DataTypes.INTEGER_VALUE(BigInt.moduloBigint(i1, i2)),DataTypes.INT)
                                                    end
                                                else raise TypeMismatch
                                            end
            |   DataTypes.EQ(e1,e2) =>      let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.INT andalso type2 = DataTypes.INT) 
                                                then
                                                    let
                                                        val DataTypes.INTEGER_VALUE(i1) = val1;
                                                        val DataTypes.INTEGER_VALUE(i2) = val2;
                                                    in 
                                                        (* (print (BigInt.showBigint(i1)); print (BigInt.showBigint(i2)); *)
                                                        if(BigInt.equalBigint(i1, i2)) then (DataTypes.TRUE,DataTypes.BOOL) else (DataTypes.FALSE,DataTypes.BOOL)
                                                    end
                                                else 
                                                    if(type1 = DataTypes.BOOL andalso type2 = DataTypes.BOOL) 
                                                    then
                                                        if(val1 = val2)
                                                        then 
                                                            (DataTypes.TRUE,DataTypes.BOOL) 
                                                        else 
                                                            (DataTypes.FALSE,DataTypes.BOOL)  
                                                    else 
                                                        if(type1 = DataTypes.RATIONAL andalso type2 = DataTypes.RATIONAL) 
                                                        then
                                                            let
                                                                val DataTypes.RATIONAL_VALUE(r1) = val1;
                                                                val DataTypes.RATIONAL_VALUE(r2) = val2;
                                                            in
                                                                if(ExpOp.equal(r1, r2)) then (DataTypes.TRUE,DataTypes.BOOL) else (DataTypes.FALSE,DataTypes.BOOL)
                                                            end
                                                        else raise TypeMismatch
                                            end
            |   DataTypes.NE(e1,e2) =>     let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.INT andalso type2 = DataTypes.INT) 
                                                then
                                                    let
                                                        val DataTypes.INTEGER_VALUE(i1) = val1;
                                                        val DataTypes.INTEGER_VALUE(i2) = val2;
                                                    in 
                                                        if(BigInt.equalBigint(i1, i2)) then (DataTypes.FALSE,DataTypes.BOOL) else (DataTypes.TRUE,DataTypes.BOOL)
                                                    end
                                                else 
                                                    if(type1 = DataTypes.BOOL andalso type2 = DataTypes.BOOL) 
                                                    then
                                                        if(val1 = val2)
                                                        then 
                                                            (DataTypes.FALSE,DataTypes.BOOL) 
                                                        else 
                                                            (DataTypes.TRUE,DataTypes.BOOL)  
                                                    else 
                                                        if(type1 = DataTypes.RATIONAL andalso type2 = DataTypes.RATIONAL) 
                                                        then
                                                            let
                                                                val DataTypes.RATIONAL_VALUE(r1) = val1;
                                                                val DataTypes.RATIONAL_VALUE(r2) = val2;
                                                            in
                                                                if(ExpOp.equal(r1, r2)) then (DataTypes.FALSE,DataTypes.BOOL) else (DataTypes.TRUE,DataTypes.BOOL)
                                                            end
                                                        else raise TypeMismatch
                                            end
            |   DataTypes.LT(e1,e2) =>      let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.INT andalso type2 = DataTypes.INT) 
                                                then
                                                    let
                                                        val DataTypes.INTEGER_VALUE(i1) = val1;
                                                        val DataTypes.INTEGER_VALUE(i2) = val2;
                                                    in 
                                                        if(BigInt.lessBigint(i1, i2)) then (DataTypes.TRUE,DataTypes.BOOL) else (DataTypes.FALSE,DataTypes.BOOL)
                                                    end
                                                else 
                                                    if(type1 = DataTypes.RATIONAL andalso type2 = DataTypes.RATIONAL) 
                                                    then
                                                        let
                                                            val DataTypes.RATIONAL_VALUE(r1) = val1;
                                                            val DataTypes.RATIONAL_VALUE(r2) = val2;
                                                        in
                                                            if(ExpOp.less(r1, r2)) then (DataTypes.TRUE,DataTypes.BOOL) else (DataTypes.FALSE,DataTypes.BOOL)
                                                        end
                                                    else raise TypeMismatch
                                            end
            
            |   DataTypes.LE(e1,e2) =>      let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.INT andalso type2 = DataTypes.INT) 
                                                then
                                                    let
                                                        val DataTypes.INTEGER_VALUE(i1) = val1;
                                                        val DataTypes.INTEGER_VALUE(i2) = val2;
                                                    in 
                                                        if(BigInt.lessBigint(i1, i2) orelse BigInt.equalBigint(i1, i2)) then (DataTypes.TRUE,DataTypes.BOOL) else (DataTypes.FALSE,DataTypes.BOOL)
                                                    end
                                                else 
                                                    if(type1 = DataTypes.RATIONAL andalso type2 = DataTypes.RATIONAL) 
                                                    then
                                                        let
                                                            val DataTypes.RATIONAL_VALUE(r1) = val1;
                                                            val DataTypes.RATIONAL_VALUE(r2) = val2;
                                                        in
                                                            if(ExpOp.less(r1, r2) orelse ExpOp.equal(r1, r2)) then (DataTypes.TRUE,DataTypes.BOOL) else (DataTypes.FALSE,DataTypes.BOOL)
                                                        end
                                                    else raise TypeMismatch
                                            end
            |   DataTypes.GT(e1,e2) =>     let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.INT andalso type2 = DataTypes.INT) 
                                                then
                                                    let
                                                        val DataTypes.INTEGER_VALUE(i1) = val1;
                                                        val DataTypes.INTEGER_VALUE(i2) = val2;
                                                    in 
                                                        if(BigInt.greaterBigint(i1, i2)) then (DataTypes.TRUE,DataTypes.BOOL) else (DataTypes.FALSE,DataTypes.BOOL)
                                                    end
                                                else 
                                                    if(type1 = DataTypes.RATIONAL andalso type2 = DataTypes.RATIONAL) 
                                                    then
                                                        let
                                                            val DataTypes.RATIONAL_VALUE(r1) = val1;
                                                            val DataTypes.RATIONAL_VALUE(r2) = val2;
                                                        in
                                                            if(ExpOp.less(r1, r2) = false andalso ExpOp.equal(r1,r2) = false) then (DataTypes.TRUE,DataTypes.BOOL) else (DataTypes.FALSE,DataTypes.BOOL)
                                                        end
                                                    else raise TypeMismatch
                                            end
            |   DataTypes.GE(e1,e2) =>      let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.INT andalso type2 = DataTypes.INT) 
                                                then
                                                    let
                                                        val DataTypes.INTEGER_VALUE(i1) = val1;
                                                        val DataTypes.INTEGER_VALUE(i2) = val2;
                                                    in 
                                                        if(BigInt.greaterBigint(i1, i2) orelse BigInt.equalBigint(i1, i2)) then (DataTypes.TRUE,DataTypes.BOOL) else (DataTypes.FALSE,DataTypes.BOOL)
                                                    end
                                                else 
                                                    if(type1 = DataTypes.RATIONAL andalso type2 = DataTypes.RATIONAL) 
                                                    then
                                                        let
                                                            val DataTypes.RATIONAL_VALUE(r1) = val1;
                                                            val DataTypes.RATIONAL_VALUE(r2) = val2;
                                                        in
                                                            if(ExpOp.less(r1, r2) = false) then (DataTypes.TRUE,DataTypes.BOOL) else (DataTypes.FALSE,DataTypes.BOOL)
                                                        end
                                                    else raise TypeMismatch
                                            end
            |   DataTypes.AND(e1,e2) =>     let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.BOOL andalso type2 = DataTypes.BOOL) 
                                                then
                                                    if(val1 = DataTypes.TRUE andalso val2 = DataTypes.TRUE) 
                                                    then 
                                                        (DataTypes.TRUE,DataTypes.BOOL) 
                                                    else 
                                                        (DataTypes.FALSE,DataTypes.BOOL)  
                                                else raise TypeMismatch
                                            end
            |   DataTypes.OR(e1,e2) =>      let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.BOOL andalso type2 = DataTypes.BOOL) 
                                                then
                                                    if(val1 = DataTypes.TRUE orelse val2 = DataTypes.TRUE) 
                                                    then 
                                                        (DataTypes.TRUE,DataTypes.BOOL) 
                                                    else 
                                                        (DataTypes.FALSE,DataTypes.BOOL)  
                                                else raise TypeMismatch
                                            end
            |   DataTypes.NOT(e1) =>        let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                            in
                                                if(type1 = DataTypes.BOOL) 
                                                then
                                                    if(val1 = DataTypes.TRUE) 
                                                    then 
                                                        (DataTypes.FALSE,DataTypes.BOOL) 
                                                    else 
                                                        (DataTypes.TRUE,DataTypes.BOOL)  
                                                else raise TypeMismatch
                                            end
            |   DataTypes.RATPLUS(e1,e2) => let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.RATIONAL andalso type2 = DataTypes.RATIONAL) 
                                                then
                                                    let
                                                        val DataTypes.RATIONAL_VALUE(r1) = val1;
                                                        val DataTypes.RATIONAL_VALUE(r2) = val2;
                                                    in
                                                        (DataTypes.RATIONAL_VALUE(ExpOp.add(r1, r2)),DataTypes.RATIONAL)
                                                    end
                                                else raise TypeMismatch
                                            end
            |   DataTypes.RATSUB(e1,e2) =>  let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.RATIONAL andalso type2 = DataTypes.RATIONAL) 
                                                then
                                                    let
                                                        val DataTypes.RATIONAL_VALUE(r1) = val1;
                                                        val DataTypes.RATIONAL_VALUE(r2) = val2;
                                                    in
                                                        (DataTypes.RATIONAL_VALUE(ExpOp.subtract(r1, r2)),DataTypes.RATIONAL)
                                                    end
                                                else raise TypeMismatch
                                            end
            |   DataTypes.RATTIMES(e1,e2)=> let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.RATIONAL andalso type2 = DataTypes.RATIONAL) 
                                                then
                                                    let
                                                        val DataTypes.RATIONAL_VALUE(r1) = val1;
                                                        val DataTypes.RATIONAL_VALUE(r2) = val2;
                                                    in
                                                        (DataTypes.RATIONAL_VALUE(ExpOp.multiply(r1, r2)),DataTypes.RATIONAL)
                                                    end
                                                else raise TypeMismatch
                                            end

            |   DataTypes.RATDIV(e1,e2)=>   let
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.RATIONAL andalso type2 = DataTypes.RATIONAL) 
                                                then
                                                    let
                                                        val DataTypes.RATIONAL_VALUE(r1) = val1;
                                                        val DataTypes.RATIONAL_VALUE(r2) = val2;
                                                    in
                                                        (DataTypes.RATIONAL_VALUE(valOf(ExpOp.divide(r1, r2))),DataTypes.RATIONAL)
                                                    end
                                                else raise TypeMismatch
                                            end
            
            |   DataTypes.NEG(e1) =>        let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                            in
                                                if(type1 = DataTypes.INT) 
                                                then
                                                    let
                                                        val DataTypes.INTEGER_VALUE(i1) = val1;
                                                    in 
                                                        (DataTypes.INTEGER_VALUE(BigInt.revSign(i1)),DataTypes.INT)
                                                    end
                                                else 
                                                    if(type1 = DataTypes.RATIONAL) 
                                                    then
                                                        let
                                                            val DataTypes.RATIONAL_VALUE(r1) = val1;
                                                        in
                                                            (DataTypes.RATIONAL_VALUE(ExpOp.neg(r1)),DataTypes.RATIONAL)
                                                        end
                                                    else raise TypeMismatch
                                            end
            |   DataTypes.INVERSE(e1) =>    let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                            in
                                                if(type1 = DataTypes.RATIONAL) 
                                                then
                                                    let
                                                        val DataTypes.RATIONAL_VALUE(r1) = val1;
                                                    in
                                                        (DataTypes.RATIONAL_VALUE(valOf(ExpOp.inverse(r1))),DataTypes.RATIONAL)
                                                    end
                                                else raise TypeMismatch
                                            end
            (* |   DataTypes.MAKERAT(e1,e2) => let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.INT andalso type2 = DataTypes.INT) 
                                                then
                                                    let
                                                        val DataTypes.INTEGER_VALUE(i1) = val1;
                                                        val DataTypes.INTEGER_VALUE(i2) = val2;
                                                    in 
                                                        (DataTypes.RATIONAL_VALUE(ExpOp.makeRational(i1, i2)),DataTypes.RATIONAL)
                                                    end
                                                else raise TypeMismatch
                                            end *)
            |   DataTypes.RAT(e1) =>        let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                            in
                                                if(type1 = DataTypes.INT) 
                                                then
                                                    let
                                                        val DataTypes.INTEGER_VALUE(i1) = val1;
                                                    in 
                                                        (DataTypes.RATIONAL_VALUE(valOf(ExpOp.rat(i1))),DataTypes.RATIONAL)
                                                    end
                                                else raise TypeMismatch
                                            end
            |   DataTypes.MAKERAT(e1,e2)=>  let
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                                val (val2, type2) = evaluateEXP(e2, scopeindex);
                                            in
                                                if(type1 = DataTypes.INT andalso type2 = DataTypes.INT) 
                                                then
                                                    let
                                                        val DataTypes.INTEGER_VALUE(i1) = val1;
                                                        val DataTypes.INTEGER_VALUE(i2) = val2;
                                                    in 
                                                        (DataTypes.RATIONAL_VALUE(valOf(ExpOp.make_rat(i1, i2))),DataTypes.RATIONAL)
                                                    end
                                                else raise TypeMismatch
                                            end
            |   DataTypes.FROMDECIMAL(e1)=> let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                            in
                                                if(type1 = DataTypes.RATIONAL) 
                                                then
                                                let
                                                    val DataTypes.STRING_VALUE(r1) = val1;
                                                in
                                                    (DataTypes.RATIONAL_VALUE(ExpOp.fromDecimal(r1)),DataTypes.RATIONAL)
                                                end
                                                else raise TypeMismatch
                                            end
            |   DataTypes.TODECIMAL(e1)=>   let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                            in
                                                if(type1 = DataTypes.RATIONAL) 
                                                then
                                                let
                                                    val DataTypes.RATIONAL_VALUE(r1) = val1;
                                                in
                                                    (DataTypes.STRING_VALUE(ExpOp.toDecimal(r1)),DataTypes.STRING)
                                                end
                                                else raise TypeMismatch
                                            end
            |   DataTypes.SHOWRAT(e1)=>     let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                            in
                                                if(type1 = DataTypes.RATIONAL) 
                                                then
                                                let
                                                    val DataTypes.RATIONAL_VALUE(r1) = val1;
                                                in
                                                    (DataTypes.STRING_VALUE(ExpOp.showRat(r1)),DataTypes.STRING)
                                                end
                                                else raise TypeMismatch
                                            end
            |   DataTypes.SHOWDECIMAL(e1)=> let 
                                                val (val1, type1) = evaluateEXP(e1, scopeindex);
                                            in
                                                if(type1 = DataTypes.RATIONAL) 
                                                then
                                                let
                                                    val DataTypes.RATIONAL_VALUE(r1) = val1;
                                                in
                                                    (DataTypes.STRING_VALUE(ExpOp.toDecimal(r1)),DataTypes.STRING)
                                                end
                                                else raise TypeMismatch
                                            end
            |   DataTypes.UNPLUS(e1) => let 
                                            val (val1, type1) = evaluateEXP(e1, scopeindex);
                                        in 
                                            if(type1 = DataTypes.INT) 
                                            then
                                                let
                                                    val DataTypes.INTEGER_VALUE(i1) = val1;
                                                in 
                                                    (DataTypes.INTEGER_VALUE(i1),DataTypes.INT)
                                                end
                                            else 
                                                if(type1 = DataTypes.RATIONAL) 
                                                then
                                                    let
                                                        val DataTypes.RATIONAL_VALUE(r1) = val1;
                                                    in
                                                        (DataTypes.RATIONAL_VALUE(r1),DataTypes.RATIONAL)
                                                    end
                                                else raise TypeMismatch
                                        end
            
        end 
    
    
    
    fun pushFrame(scopetable, scopeindex, procdeflist) = 
        let
            val x = HashTable.listItemsi(scopetable);
            val frame = List.map (fn (i, vartype) => (i, setZero(vartype))) x;
            val hashFrame: (string, DataTypes.EXP) HashTable.hash_table = HashTable.mkTable (HashString.hashString, op=) (10, Fail "not found")
            val hashProc: (string, DataTypes.BLOCK) HashTable.hash_table = HashTable.mkTable (HashString.hashString, op=) (10, Fail "not found")

        in
            (List.app (fn (i, varval) => (HashTable.insert hashFrame (i, varval))) frame;
            List.app (fn DataTypes.PROCDEF(i, procdef) => (HashTable.insert hashProc (i, procdef))) procdeflist;
            (* print("adding frame with scopeid "^Int.toString(scopeindex)^" to the callstack \n"); *)
            callstack := CALLFRAME(!callframe_id + 1,scopeindex,hashFrame,hashProc):: !callstack;
            callframe_id := !callframe_id + 1)
        end


    fun set(var, callstack, scopeindex, value) = 
        if(null (callstack))then 
            raise UndeclaredBinding
        else
            let
                val CALLFRAME(callframe_id, scope_index, currentFrame, proctable) = hd(callstack);
                val parentScope = HashTable.find parentTable (Int.toString(scopeindex))
            in
                if(HashTable.find currentFrame var = NONE andalso scope_index=scopeindex) then
                    if(parentScope <> NONE) then
                        (* (print("in set: searching in parentscope "^Int.toString(valOf(parentScope))^"\n"); *)
                        (set(var, tl(callstack),valOf(parentScope), value))
                    else
                        raise UndeclaredBinding

                else 
                    if (HashTable.find currentFrame var = NONE andalso scope_index<>scopeindex) then
                        (* (print("in set: searching further down, the current callframe has scope "^(Int.toString(scopeindex))^"\n"); *)
                        (set(var, tl(callstack), scopeindex, value))
                    else
                        if(isSome(HashTable.find currentFrame var) andalso scope_index = scopeindex) then
                            (* (print("in set: found value "^var^ " in current scope "^Int.toString(scopeindex)^"\n"); *)
                            (HashTable.insert currentFrame (var,value))     
                        else    
                            (* (print("in set: searching further down, the current callframe has scope "^(Int.toString(scope_index))^"\n"); *)
                            (set(var, tl(callstack), scopeindex, value))    
            end

    fun runProcedure(blk : DataTypes.BLOCK) = 
        let
            val DataTypes.BLOCK(declist, procdeflist, cmdlist, scope_table, scopeindex) = blk;
            val DataTypes.SCOPE_TABLE(scopetable) = scope_table;
        in
            (pushFrame(scopetable, !scopeindex, procdeflist);
            List.app (fn (cmd) => runCommand(cmd, !scopeindex)) cmdlist;
            (* print("popping stack with scopeid"^Int.toString(!scopeindex)^"\n"); *)
            callstack := tl(!callstack);
            callframe_id := !callframe_id - 1)
        end
    
    
    and runCommand(cmd: DataTypes.CMD , scopeindex: int) = 
        case cmd of
            DataTypes.SET(var, exp) =>        let
                                        val q = evaluateEXP(exp, scopeindex);
                                        val x = #1 q;
                                    in 
                                        set(var, !callstack, scopeindex, x)
                                    end
        |   DataTypes.IFFI(exp,cmd1,cmd2)=>   let 
                                        val q = evaluateEXP(exp, scopeindex);
                                        val x = #1 q;
                                        val vartype = #2 q;                                    
                                    in  
                                        if(vartype<>DataTypes.BOOL) then
                                            raise InvalidBooleanExpressioninIf
                                        else
                                            if(x = DataTypes.TRUE) then 
                                                List.app (fn (cmd) => runCommand(cmd, scopeindex)) cmd1
                                            else 
                                                List.app (fn (cmd) => runCommand(cmd, scopeindex)) cmd2
                                    end

        |   DataTypes.Print(exp)=>            let       
                                        val q = evaluateEXP(exp, scopeindex);
                                        val x = #1 q;
                                    in  
                                        case x of
                                            DataTypes.TRUE => writeFile("tt"^"\n")
                                        |   DataTypes.FALSE => writeFile("ff"^"\n")
                                        |   DataTypes.INTEGER_VALUE(i) => writeFile(BigInt.showBigint(i)^"\n")
                                        |   DataTypes.RATIONAL_VALUE(i) => writeFile(ExpOp.toDecimal(i)^"\n")
                                        |   DataTypes.STRING_VALUE(i) => writeFile(i^"\n")
                                    end

        |   DataTypes.WHILE(exp,commandlist)=> while 
                                        (let
                                            val q = evaluateEXP(exp, scopeindex);
                                            val x = #1 q;
                                            val vartype = #2 q;
                                        in
                                            if(vartype<>DataTypes.BOOL) then
                                                raise InvalidBooleanExpressioninWhile
                                            else
                                                if(x = DataTypes.TRUE) then 
                                                    true
                                                else 
                                                    false
                                        end)
                                        do
                                            (List.app (fn (cmd) => runCommand(cmd, scopeindex)) commandlist)
                                    
        |   DataTypes.Call(procname) => let 
                                            val x = proclookup(procname, !callstack, scopeindex)
                                        in
                                            (* (print("calling procedure: "^procname^"\n"); *)
                                            runProcedure(x)
                                        end
        |   DataTypes.Read(s) =>  (print("Please enter the value of variable "^s^" : \n"); 

                            let 
                            val str =   valOf (TextIO.inputLine TextIO.stdIn);
                            val snew = String.substring(str, 0, String.size(str)-1);
                            in                            case getType(lookup(s, !callstack, scopeindex)) of
                                DataTypes.INT => set(s, !callstack, scopeindex, DataTypes.INTEGER_VALUE(BigInt.toBigint(snew)))
                            |   DataTypes.RATIONAL => set(s, !callstack, scopeindex, DataTypes.RATIONAL_VALUE(ExpOp.fromDecimal(snew)))
                            |   DataTypes.BOOL => if(snew = "tt") then (set(s, !callstack, scopeindex, DataTypes.TRUE)) else if(snew="ff") then set(s, !callstack, scopeindex, DataTypes.FALSE) else raise InvalidBooleanInput
                            |   _ => raise TypeMismatch
                            end)
    
    
    
    
    fun runProgram(blk : DataTypes.BLOCK) = 
        let
            val DataTypes.BLOCK(declist, procdeflist, cmdlist, scope_table, scopeindex) = blk;
            val DataTypes.SCOPE_TABLE(scopetable) = scope_table;
        in
            (pushFrame(scopetable, !scopeindex, procdeflist);
            List.app (fn (cmd) => runCommand(cmd, !scopeindex)) cmdlist;
            callstack := tl(!callstack);
            callframe_id := !callframe_id - 1)
        end
    
    
    
    fun interpret(input, output) = (
                                   inputFile := input;
                                   outputFile := output;
                                   initFile();
                                   HashTable.clear parentTable; 
                                   callframe_id = ref 0; 
                                   HashTable.clear globalScopedSymbolTable; 
                                   callstack = ref[]; 
                                   runProgram(retBlock (!inputFile));
                                   closeFile())
end;