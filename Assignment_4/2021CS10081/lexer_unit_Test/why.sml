
val fileName = "prog.txt"
val inStream = TextIO.openIn fileName
        val grab : int -> string = fn
        n => if TextIO.endOfStream inStream
            then ""
            else TextIO.inputN (inStream,n)
val k = MyLex.makeLexer grab