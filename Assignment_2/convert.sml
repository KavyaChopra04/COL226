(*Sorting input and output*)
fun readLines(inputStream, lines) = 
    let val line = TextIO.inputLine(inputStream)
    in
    if(line=NONE) then 
    lines
    else 
    readLines(inputStream, valOf(line) ::lines)
    end

fun readFile(inputFile) = 
    let
    val inputStream = TextIO.openIn(inputFile)
    val listOfLines = readLines((inputStream), [])
    in 
    rev(listOfLines)
    end

fun closeFile(inputStream)=
    TextIO.closeIn(inputStream)

fun writeFile(outputFile, listOfLines) =
    let
    val outputStream = TextIO.openOut(outputFile)
    val _ = List.app(fn line => TextIO.output(outputStream, line)) listOfLines
    in
    TextIO.closeOut(outputStream)
    end

(*Processing input*)
fun emptyStack(stack, string) = 
    if(null stack)
    then string
    else case hd(stack) of 
                 "<li>" => emptyStack(stack, string^"</li>")
                | "<ul>" => emptyStack(stack, string^"</ul>")
                | "<ol>" => emptyStack(stack, string^"</ol>")
                | "<p>" => emptyStack(stack, string^"</p>")
                | "<blockquote>" => emptyStack(stack, string^"</blockquote>")
fun allHyphens(string)= 
    if(string = "")then true
    else
        if(String.isPrefix " " string)then 
            allHyphens(String.extract(string, 1, NONE))
        else false
fun parseHorizontalRule(string) =
    if(allHyphens(string) andalso size(string) > 2) then "<hr>"
    else string

fun trimw(string) = 
    if (String.isPrefix " " string) then trimw(String.extract (string, 1, NONE))
    else string
fun parseHeader(line) = 
    if(String.isPrefix "######" line)
    then "<h6>"^String.extract (line, 6, SOME (size(line) - 7))^"</h6>"^String.extract (line, size(line)-1, NONE)
    else if (String.isPrefix "#####" line)
    then "<h5>"^String.extract (line, 5, SOME (size(line) - 6))^"</h5>"^String.extract (line, size(line)-1, NONE)
    else if (String.isPrefix "####" line)
    then "<h4>"^String.extract (line, 4, SOME (size(line) - 5))^"</h4>"^String.extract (line, size(line)-1, NONE)
    else if (String.isPrefix "###" line)
    then "<h3>"^String.extract (line, 3, SOME (size(line) - 4))^"</h3>"^String.extract (line, size(line)-1, NONE)
    else if (String.isPrefix "##" line )
    then "<h2>"^String.extract (line, 2, SOME (size(line) - 3))^"</h2>"^String.extract (line, size(line)-1, NONE)
    else if (String.isPrefix "#" line)
    then "<h1>"^String.extract (line, 1, SOME (size(line) - 2))^"</h1>"^String.extract (line, size(line)-1, NONE)
    else line 

fun getIndex(string, substring, depth) = 
    if (size(string) < size(substring)) then ~1
    else
    if(String.isPrefix substring string)
    then depth
    else getIndex(String.extract(string, 1, NONE), substring, depth+1)

fun parseBold (string) =
    if (String.isSubstring "**" string) then
    let
    val firstIndex = getIndex(String.extract(string, 0, NONE) , "**", 0)
    val secondIndex = getIndex(String.extract(string, firstIndex+2, NONE) , "**", 0)
    in
    if(secondIndex = ~1) 
    then string
    else
    String.substring(string, 0, firstIndex) ^ "<strong>" ^ String.substring(string, firstIndex+2, secondIndex) ^ "</strong>" ^ parseBold(String.extract(string, firstIndex +2+ secondIndex+2, NONE))
    end
    else string
fun parseitalic(string) = 
    if (String.isSubstring "*" string) then
    let
    val firstIndex = getIndex(String.extract(string, 0, NONE) , "*", 0)
    val secondIndex = getIndex(String.extract(string, firstIndex+1, NONE) , "*", 0)
    in
    if(secondIndex = ~1) 
    then string
    else
    String.substring(string, 0, firstIndex) ^ "<em>" ^ String.substring(string, firstIndex+1, secondIndex) ^ "</em>" ^ parseitalic(String.extract(string, firstIndex+1+secondIndex+1, NONE))
    end
    else string
fun parseUnderline(string) =

    if (String.isSubstring "_" string) then
    let
    val firstIndex = getIndex(String.extract(string, 0, NONE) , "*", 0)
    val secondIndex = getIndex(String.extract(string, firstIndex+1, NONE) , "*", 0)
    in
    if(secondIndex = ~1) 
    then string
    else
    String.substring(string, 0, firstIndex) ^ "<u>" ^ String.substring(string, firstIndex+1, secondIndex) ^ "</u>" ^ parseUnderline(String.extract(string, firstIndex+secondIndex+1, NONE))
    end
    else string


fun processList( listOfLines, prevLine, stack, finalList) = 
    if (null listOfLines) then
    finalList
    else
    let
    val current = hd(listOfLines)
    val headParsed = parseHeader(current)
    val boldParsed = parseBold(headParsed)
    val italicParsed = parseitalic(boldParsed)
    val line = parseHorizontalRule(italicParsed)
    val trimline=trimw(line)
    val trimprevLine=trimw(prevLine)
    in
    if(headParsed<>current)
    then
    processList(tl(listOfLines), line, stack, emptyStack(stack, "")^"\n"::finalList)
    else if(trimline = "\n") then
        if (null stack) then  processList(tl(listOfLines), line, stack, "\n"::finalList)
        else 
            case hd(stack) of 
                 "<li>" => processList(tl(listOfLines), line, tl stack, "</li>"::finalList)
                | "<ul>" => processList(tl(listOfLines), line, tl stack, "</ul>"::finalList)
                | "<ol>" => processList(tl(listOfLines), line, tl stack, "</ol>"::finalList)
                | "<p>" => processList(tl(listOfLines), line, tl stack, "</p> <p>"::finalList)
                | "<blockquote>" => processList(tl(listOfLines), line, tl stack, "</blockquote>"::finalList)
    else
    if (String.isPrefix "-" trimline) then  
            if(null(stack)) then processList(tl(listOfLines), line, "<li>"::"<ul>"::stack, "<ul>\n"^"<li>"^String.extract (trimline, 1, SOME (size(trimline) - 2))^String.extract (line, size(line)-1, NONE) :: finalList) else
            if (hd(stack) = "<ul>") 
            then processList(tl(listOfLines), line, "<li>"::stack, "<li>"^String.extract (trimline, 1, SOME (size(trimline) - 2))^String.extract (line, size(line)-1, NONE) :: finalList)
            else if (hd(stack) = "<li>")
            then processList(tl(listOfLines), line, stack, "</li>\n <li>"^String.extract (trimline, 1, SOME (size(trimline) - 2))^String.extract (line, size(line)-1, NONE) :: finalList)
        else processList(tl(listOfLines), line, "<li>"::"<ul>"::stack, "<ul>\n"^"<li>"^String.extract (line, 1, SOME (size(line) - 2))^String.extract (line, size(line)-1, NONE) :: finalList)
    else if (String.isPrefix ">" trimline) then processList(tl(listOfLines), line, stack, "<blockquote>"^String.extract (line, 1, SOME (size(line) - 2))^"</blockquote>"^String.extract (line, size(line)-1, NONE) :: finalList)
    else processList(tl(listOfLines), line, stack, line :: finalList)
    end
    
(*final write to output file*)
fun initFunc(inputFile, outputFile) = 
    let
    val listOfLines = readFile(inputFile)
    val finalList = processList(listOfLines, "", [], [])
    in
    writeFile(outputFile, rev(finalList))
    end