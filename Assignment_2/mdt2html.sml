




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
fun pref(string, other) = 
    if(size(string) < size(other)) then false else 
    if (String.isPrefix other string) then true
    else pref(String.extract(string, 1, NONE), other)
fun getIndex(string, substring, depth) = 
    if (size(string) < size(substring)) then ~1
    else
    if(String.isPrefix substring string)
    then depth
    else getIndex(String.extract(string, 1, NONE), substring, depth+1)
fun extractLink(string) = 
    let 
    val firstIndex = getIndex(string , "]", 0)
    val secondIndex = getIndex(string, ":", 0)
    val name = String.extract(string, 1, SOME (firstIndex - 1))
    val link = String.extract(string, secondIndex + 1, SOME (size(string) - 2 - secondIndex))
    in 
    ("["^name^"]", link)
    end
fun extractLinks(listOfLines, linkarr, finalList) = 
    if null listOfLines then (linkarr, finalList)
    else if (String.isPrefix "[" (hd(listOfLines)) andalso String.isSubstring "]:" (hd(listOfLines))) then extractLinks(tl(listOfLines),  extractLink(hd(listOfLines))::linkarr, finalList)
    else extractLinks(tl(listOfLines), linkarr, hd(listOfLines)::finalList)
fun replaceLink(string, linkarr: (string*string) list, linkarr2) = 
    if (null linkarr) then string
    else
    let 
    val current = hd(linkarr)
    val firstIndex = getIndex(String.extract(string, 0, NONE) , "[", 0)
    val secondIndex = getIndex(String.extract(string, 0, NONE) , "]", 0)
    val thirdIndex = firstIndex + 1 + getIndex(String.extract(string, firstIndex + 1, NONE) , "[", 0)
    val fourthIndex =secondIndex + 1 + getIndex(String.extract(string, secondIndex + 1, NONE) , "]", 0)
    val name = #1 current
    val link = #2 current
    in 
    if(pref(string, name)) then
    String.extract(string, 0, SOME firstIndex) ^ "<a href= \" " ^ link ^ "\">" ^ String.substring(string, firstIndex + 1, secondIndex - 1 - firstIndex) ^ "</a>" ^ replaceLink(String.extract(string, fourthIndex + 1, NONE), linkarr2, linkarr2)
    else replaceLink(string, tl(linkarr), linkarr2)
    end

fun contain(string, linkarr: (string*string) list) = 
    if (null linkarr) then false
    else
    let 
    val current = hd(linkarr)
    val newl = contain(string, tl(linkarr))
    val name = #1 current
    in 
    if(pref(string, name)) then true
    else newl
    end

fun replaceLinks(listOfLines, linksarr, finalList) = 
    if null listOfLines then finalList
    else
    let 
    val current = hd(listOfLines)
    in 
    if(contain(current, linksarr)) then 
    let 
    val newc = replaceLink(current, linksarr, linksarr)
    in 
    replaceLinks(tl(listOfLines), linksarr, newc::finalList)
    end
    else replaceLinks(tl(listOfLines), linksarr, current::finalList)
    end


fun parseRow(string, list) = 
    let
        val firstIndex = getIndex(String.extract(string, 0, NONE) , "|", 0)
    in
        if firstIndex = ~1 then string::list
        else parseRow(String.extract(string, firstIndex+1, NONE), String.extract(string, 0, SOME firstIndex)::list)
    end
fun tablerow(list) = 
    if null(list) then ""
    else "<TD>"^hd(list)^"</TD>"^tablerow(tl(list))
fun countShifts(string, num) = 
    if(size(string) = 0) then (string, 0)
    else if(String.isPrefix " " string orelse String.isPrefix " " string) then countShifts(String.extract(string, 1, NONE), num)
    else if String.isPrefix ">" string then countShifts(String.extract(string, 1, NONE), num+1)
    else (string, num)

fun popfin(list, string)= 
    if null(list) then ([], string)
    else if(hd(list) = "<ul>") then popfin(tl(list), string^"</ul>")
    else if(hd(list) = "<ol>") then popfin(tl(list), string^"</ol>")
    else if(hd(list) = "<li>") then popfin(tl(list), string^"</li>")
    else popfin(tl(list), string)

fun poplist(list, string) = 
    if(null list) then (list, string)
    else
    if(hd(list) = "<ul>") then poplist(tl(list), string^"</ul>")
    else if(hd(list) = "<ol>") then poplist(tl(list), string^"</ol>")
    else (list, string)
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
    if(string = "\n")then true
    else
        if(String.isPrefix "-" string)then 
            allHyphens(String.extract(string, 1, NONE))
        else false
fun parseHorizontalRule(string) =
    if(allHyphens(string) andalso size(string) > 2) then "<hr>"
    else string

fun trimw(string, n) = 
    if(size(string) = 0) then (string, 0) 
    else    
    if (String.isPrefix " " string) then trimw(String.extract (string, 1, NONE), n+1)
    else if (String.isPrefix "\t" string) then trimw(String.extract (string, 1, NONE), n+4)
    else (string, n)
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

fun remUnderline(string) = 
    if (String.isSubstring "_" string) then
    let
    val firstIndex = getIndex(String.extract(string, 0, NONE) , "_", 0)
    in
    if(firstIndex = ~1) 
    then string
    else
    String.substring(string, 0, firstIndex) ^ String.extract(string, firstIndex+1, NONE) 
    end
    else string

fun parseUnderline(string) =

    if (String.isSubstring "_" string) then
    let
    val firstIndex = getIndex(String.extract(string, 0, NONE) , "_", 0)
    val secondIndex = getIndex(String.extract(string, firstIndex+1, NONE) , "_", 0)
    in
    if(secondIndex = ~1) 
    then remUnderline(string)
    else
    String.substring(string, 0, firstIndex) ^ "<u>" ^ String.substring(string, firstIndex+1, secondIndex) ^ "</u>" ^ parseUnderline(String.extract(string, firstIndex+secondIndex+1, NONE))
    end
    else string
fun numstring(livst: char list, stger) = 
    if(null(livst)) then (true, "") else
    let 
    val valc = hd(livst) 
    in 
    if (Char.isDigit valc) then numstring(tl livst, stger) 
    else  if(Char.ord valc = 46) then (true, String.implode(tl(livst))) 
    else (false, String.implode(livst))
    end

fun parse(string) = 
    if (String.isSubstring "<" string orelse String.isSubstring">" string) then
    if (String.isPrefix "<http" string) then
    "&lt"^parse(String.extract(string, 1, NONE))
    else if (String.isPrefix ">" string) then
    "&gt;"^parse(String.extract(string, 1, NONE))
    else String.extract(string, 0, SOME 1)^parse(String.extract(string, 1, NONE))
    else string

fun parseCodeblocks(current, codeblock) = 
    let 
    val tup = trimw(current, 0) 
    val numspace = #2 tup
    val trim = #1 tup
    val q= numstring(String.explode(trim), "")
    val r = #1 q
    in 
    if (String.isPrefix "-" current orelse r ) then (current, codeblock)
    else if (codeblock = false  andalso numspace >= 12 ) then
         let val codeblock2=true 
         in 
         ("<pre><code>"^parse(current), true)
         end 
    else if (codeblock = true andalso numspace < 12) then
        (current^"</code></pre>", false)
    else (current, codeblock)
    end
fun processList( listOfLines, prevLine, stack, finalList, liststack, parastate, codeblock) = 
    if (null listOfLines) then
    let 
    val x = popfin(stack, "")
    val finalstr = #2 x
    in 
    finalstr :: finalList
    end
    else
    let
        val current = hd(listOfLines)
        val headParsed = parseHeader(current)
        val boldParsed = parseBold(headParsed)
        val italicParsed = parseitalic(boldParsed)
        val line = parseUnderline(parseHorizontalRule(italicParsed))
        val trim= trimw(line,0)
        val trimline = #1 trim
        val numspace = #2 trim
        val trimprev= trimw(prevLine,0)
        val trimprevline = #1 trimprev
        val numspaceprev= #2 trimprev
        val k= numstring(String.explode(trimline), true)
    in
    
    if(null liststack) then  
        if (String.isPrefix "-" trimline) then processList(tl(listOfLines), line, "<li>"::"<ul>"::stack, "<ul>\n"^"<li>"^String.extract (trimline, 1, NONE):: finalList, ("<ul>", numspace)::liststack, parastate, codeblock)
         else   
            if (#1 k) then processList(tl(listOfLines), line, "<li>"::"<ol>"::stack, "<ol>\n"^"<li>"^(#2 k):: finalList, ("<ol>", numspace)::liststack, parastate, codeblock)
            else
                            let
                              val firstc = hd(String.explode(trimline))
                            in
                              if (parastate = false andalso Char.isAlpha firstc andalso numspace < 4) then 
                                let val parastate2=true 
                                in 
                                processList(tl(listOfLines), line, stack, "<p>"^String.extract (trimline, 0, NONE) :: finalList, liststack, parastate2, codeblock) 
                                end
                             else if (codeblock = false  andalso numspace >= 4) then
                             let val codeblock2=true 
                                in 
                                processList(tl(listOfLines), line, stack, "<pre><code>"^String.extract (trimline, 0, NONE) :: finalList, liststack, parastate, codeblock2) 
                                end
                              else if (trimline = "\n" andalso parastate = true) then 
                                    let val parastate2=false 
                                    in 
                                    processList(tl(listOfLines), line, stack, "</p>"^String.extract (trimline, 0, NONE) :: finalList, liststack, parastate2, codeblock) 
                                    end
                                else if (trimline = "\n" andalso codeblock = true) then 
                                    let val codeblock2=false 
                                    in 
                                    processList(tl(listOfLines), line, stack, "</code></pre>"^String.extract (trimline, 0, NONE) :: finalList, liststack, parastate, codeblock2) 
                                    end
                                else processList (tl(listOfLines), line, stack, line :: finalList, liststack, parastate, codeblock)
                                end
        else if(String.isPrefix "#" current orelse String.isPrefix "<p" trimline) 
            then 
            let 
            val x = popfin(stack, "")
            val x1 = #1 x
            val x2 = #2 x
            in 
            processList(tl(listOfLines), line, x1,  x2^"\n"^String.extract (trimline, 0, NONE) :: finalList, [], parastate, codeblock)
            end
    else
    if (String.isPrefix "-" trimline) then 
            if (hd(stack) = "<ul>") then processList(tl(listOfLines), line, "<li>"::stack, "<li>"^String.extract (trimline, 0, NONE) :: finalList,liststack, parastate, codeblock)
            else 
            if (hd(stack) = "<li>" ) then 
            let 
                val y = hd(liststack)
                val y1 = #2 y
            in 
                if(y1= numspace) then processList(tl(listOfLines), line, stack, "</li>\n\n<li>"^String.extract (trimline, 1, NONE) :: finalList, liststack, parastate, codeblock)
                else if (y1 < numspace) then processList(tl(listOfLines), line, "<li>"::"<ul>"::stack, "<ul>\n"^"<li>"^String.extract (trimline, 1, NONE) :: finalList, ("<ul>", numspace)::liststack, parastate, codeblock)
                else 
                    let 
                    val x = hd(liststack)
                    val x1 = #1 x
                
                    in 
                    if x1 = "<ol>" then processList(tl(listOfLines), line, tl(tl(stack)), "</li></ol></li>\n\n"^"<li>"^String.extract (trimline, 1, NONE) :: finalList, tl(liststack), parastate, codeblock)
                    else processList(tl(listOfLines), line, tl(tl(stack)), "</li></ul></li>\n"^"<li>"^String.extract (trimline, 1, NONE) :: finalList, tl(liststack), parastate, codeblock)
                    end 
            end
            else processList(tl(listOfLines), line, "<li>"::"<ul>"::stack, "<ul>\n"^"<li>"^String.extract (trimline, 0, NONE)^String.extract (line, size(line)-1, NONE) :: finalList, ("<ul>", numspace)::liststack, parastate, codeblock)
            
    else if (String.isPrefix "-" trimline = false) then 
         if (#1 k) then 
            if (hd(stack) = "<ol>") then processList(tl(listOfLines), line, "<li>"::stack, "<li>"^ #2 k :: finalList,liststack, parastate, codeblock)
            else 
            if (hd(stack) = "<li>" ) then 
            let 
                val y = hd(liststack)
                val y1 = #2 y
                val q = prevLine
            in 
                if(y1= numspace) then processList(tl(listOfLines), line, stack, "</li>\n<li>"^ #2 k :: finalList, liststack, parastate, codeblock)
                else if (y1 < numspace) then processList(tl(listOfLines), line, "<li>"::"<ol>"::stack, line^prevLine^"<ol>\n"^"<li>"^ #2 k:: finalList, ("<ol>", numspace)::liststack, parastate, codeblock)
                else 
                    let 
                    val x = hd(liststack)
                    val x1 = #1 x
                    in 
                    if x1 = "<ol>" then processList(tl(listOfLines), line, tl(tl(stack)), "</li></ol></li>\n"^"<li>"^ #2 k :: finalList, tl(liststack), parastate, codeblock)
                    else processList(tl(listOfLines), line, tl(tl(stack)), "</li></ul></li>\n\n"^"<li>"^ #2 k :: finalList, tl(liststack), parastate, codeblock)
                    end 
            end
            else processList(tl(listOfLines), line, "<li>"::"<ol>"::stack, "<ol>\n"^"<li>"^ #2 k :: finalList, ("<ul>", numspace)::liststack, parastate, codeblock)
        else let val q = parseCodeblocks(line, codeblock)
        val r = #1 q
        val s = #2 q
        in processList(tl(listOfLines), r, stack, r:: finalList, liststack, parastate, s) end

    else processList(tl(listOfLines), line, stack, line :: finalList, liststack, parastate, codeblock)
    end
fun processMore(listOfLines, tableon, finalList) = 
    if null listOfLines then finalList
    else
    let 
    val current = hd(listOfLines)
    val trim = trimw(current, 0) 
    val numspace = #2 trim
    val line = #1 trim
    val sct = countShifts(line, 0)
    val shct = #2 sct
    val snew = #1 sct
    in 
    if(line = "<<\n") then processMore(tl(listOfLines), true, "<CENTER><TABLE border=\"1\" >\n"::finalList)
    else if(tableon) then
    if(line = ">>\n") then processMore(tl(listOfLines), false,  "</TABLE></CENTER>\n"::finalList)
    else if (shct = 0) then
    let 
    val c = #"|"
    val newl= rev(parseRow(line, []))
    in
    processMore(tl(listOfLines), tableon,  "<TR>"^tablerow(newl)^"</TR>\n"::finalList)
    end
    else processMore(tl(listOfLines), tableon,  current::finalList)
    else processMore(tl(listOfLines), tableon,  current::finalList)
    end

fun stringgen(num) = 
    if num = 0 then ""
    else if num = 1 then "<blockquote>"
    else "<blockquote>"^stringgen(num-1)

fun revgen(num) = 
    if num = 0 then ""
    else if num = 1 then "</blockquote>"
    else "</blockquote>"^revgen(num-1)


fun processBlockquote(listOflines, blockstate, finalList) = 
    if (null listOflines andalso blockstate <> 0) then "</p>"^revgen(blockstate) :: finalList
    else if (null listOflines andalso blockstate = 0) then finalList
    else
    let 
    val current = hd(listOflines)
    val trim = trimw(current, 0) 
    val numspace = #2 trim
    val snew = #1 trim
    val sct = countShifts(snew, 0)
    val shct = #2 sct
    val  line = #1 sct
    in 
    if (numspace>0 ) then processBlockquote(tl(listOflines),  0, current::finalList)
    else if (shct = 0 andalso blockstate = 0) then processBlockquote(tl(listOflines), 0, line::finalList)
    else if (shct = 0 andalso blockstate<>0) then processBlockquote(tl(listOflines), 0,"</p>"^revgen(blockstate)^line::finalList)
    else if (shct <> 0) then
        if(blockstate = 0) then processBlockquote(tl(listOflines), shct, stringgen(shct)^"<p>"^line::finalList)
        else if (blockstate < shct) then processBlockquote(tl(listOflines), shct, "</p>"^stringgen(shct-blockstate)^"<p>"^line::finalList)
        else if (blockstate  > shct) then processBlockquote(tl(listOflines), shct, "</p>"^revgen(blockstate-shct)^"<p>"^line::finalList)
        else if (blockstate = shct) then processBlockquote(tl(listOflines), shct, line::finalList)
        else processBlockquote(tl(listOflines), blockstate, line::finalList)
        else processBlockquote(tl(listOflines), blockstate, "<blockquote>\n"::finalList)
    end


(*final write to output file*)
fun initFunc(inputFile, outputFile) = 
    let

    val listOfLines = readFile(inputFile)
    val tup = extractLinks(listOfLines, [], [])
    val links = #1 tup 
    val finalList = #2 tup
    val finalList2 = replaceLinks(rev(finalList), links, [])
    val finalList3 = processList(rev(finalList2), "", [], [], [], false, false)
    val finalList4 = processMore(rev(finalList3), false, []) 
    val finalList5 = processBlockquote(rev(finalList4), 0, [])
    in
    writeFile(outputFile, rev(finalList5))
    end

fun mdt2html(input) = 
    let
    val inputroot = String.substring(input, 0, String.size(input)-3 )
    val output = inputroot^"html"
    in
    initFunc(input, output)
    end


