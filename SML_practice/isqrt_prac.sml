fun charList(s) = 
	String.explode(s);


fun numList(xs)=
	if null(xs)
	then []
	else 
		let 
			val fe=Char.ord(hd xs)-48
		in
			fe::numList(tl xs)
		end
fun numtoString(xs: int list, ys: char list)= 
if(null(xs)) then String.implode(ys) else numtoString(tl xs, chr(hd(xs)+48)::ys)

fun trimzero(xs: int list) = 
	if null(xs) then [] else
	if hd(xs) = 0 then trimzero(tl xs) else xs
fun numStr(s) = numList(charList(s));

fun addLists(xs: int list, ys: int list, carry: int) = 
	if null(xs) andalso null(ys)
	then 
		if carry=1 then [1] else []
	else
		if null(ys) then 
				  ((hd(xs) + carry) mod 10)::addLists(tl xs, ys, (hd(xs) + carry) div 10)
		else if null(xs) then
				   ((hd(ys) + carry) mod 10)::addLists(xs,tl ys,( hd(ys) + carry) div 10)
		else
				   ((hd(ys) + hd(xs)  + carry) mod 10)::addLists(tl xs,tl ys,(hd(ys) + hd(xs) +  carry) div 10)

fun multBy(xs: int list, y: int, carry: int) =
	if null(xs) then 
		if carry=0 then [] else [carry]
	else 
		(hd(xs)*y + carry) mod 10 :: multBy(tl(xs), y, (hd(xs)*y + carry) div 10)

fun cmp(xs : int list, num : int list) = (*returns true if |num| >= |xs|*) 
	if (length(xs) < length(num)) then true
	else if length(xs) > length(num)
	 then false
	else 
	 if null(xs) andalso null(num) then true
	 else 
	    if(hd(num) >hd(xs)) then true
	    else if (hd(num) < hd (xs)) then false
		else cmp(tl xs, tl num)

fun findDigit(xs: int list, num: int, dividend: int list) =
	let val newl = num ::xs
	    val sublist = trimzero(rev(multBy(newl,num, 0)))
	in 	
		if cmp(sublist, dividend) then (num,sublist)
		else findDigit(xs, num-1,dividend)
	end

fun subtractLists(bigs: int list, smalls: int list, borrow: int) =
	if(null(bigs)) then []
	else if null(smalls) then
		if(hd(bigs) + borrow >=0) then (hd(bigs)+borrow)::subtractLists(tl bigs, smalls, 0)
		else (hd(bigs) + borrow + 10)::subtractLists(tl bigs, smalls, ~1)
	else
			
		if(hd(bigs) -hd(smalls) + borrow >=0) then (hd(bigs)-hd(smalls)+borrow)::subtractLists(tl bigs, tl smalls, 0)
		else (hd(bigs) -hd(smalls) + borrow + 10)::subtractLists(tl bigs, tl smalls, ~1)

fun joinLists(fronts: int list, backs: int list) = 
	if(null(fronts)) then backs
	else hd(fronts)::joinLists(tl fronts, backs)
fun processlist(xs: int list) = 
	if(null(xs)) then []
	else if (length(xs) mod 2 =1) then [hd(xs)]::processlist(tl xs)
	else [hd xs, hd(tl(xs))] :: processlist(tl(tl(xs)))
fun doubleList(xs : int list) = multBy(xs, 2, 0)
fun calcroot(imdiv : int list, furdiv : int list list, root: int list) = 
	let
	
	val p = findDigit(doubleList(root), 9, trimzero(imdiv))
	val ls_im=trimzero(rev(subtractLists(rev(imdiv), rev(#2 p), 0)))
	in
	if(null(furdiv)) then (#1 p::root, ls_im) else
	calcroot(joinLists(ls_im,  hd(furdiv)), tl furdiv, (#1 p)::root)
	end
fun isqrtld(s : string) = 
	let val k=  processlist(numList(charList(s))) (*add exception for empty string*)
	val anspr = calcroot(hd k,tl k, [])
	in if null(#2 anspr) then ((numtoString((#1 anspr), [])), numtoString([0], [])) else
	(numtoString((#1 anspr), []), numtoString(rev(#2 anspr),[]))
	end
	
		
	
