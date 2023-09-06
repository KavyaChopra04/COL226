signature BIGINT = 
    sig
        type bigint = int list * int
        exception NegativePower
        exception ZeroDivision
        exception InvalidIntegerInput
        val repZero: bigint
        val extlist: bigint -> int list
        val calcRecPart: bigint * bigint * int list list * int list -> string * string
        val toBigint: string -> bigint
        val revSign: bigint -> bigint
        val addBigint  : bigint * bigint -> bigint
        val subtractBigint  : bigint * bigint -> bigint
        val multiplyBigint : bigint * bigint -> bigint
        val divideBigint : bigint * bigint -> bigint
        val moduloBigint : bigint * bigint -> bigint
        val equalBigint : bigint * bigint -> bool
        val lessBigint : bigint * bigint -> bool
        val greaterBigint : bigint * bigint -> bool
        val showBigint : bigint -> string
        val calcGCD : bigint * bigint -> bigint
        val power10Bigint : int * int list -> bigint
    end
structure BigInt : BIGINT =
    struct 
        exception ZeroDivision
        exception NegativePower
        exception InvalidIntegerInput
        (*Helper functions*)
        fun listpreindex(l, i) = 
            if i = 0 then []
            else hd(l)::listpreindex(tl l, i-1)
        fun listpostindex(l, i) =
            if i = 0 then l
            else listpostindex(tl l, i-1)
        fun listToString(l) = 
            implode (map (fn s => String.sub(s, 0)) (map Int.toString l))
        fun findel(x,l) = 
            if(null(l)) then false
            else 
                if(hd(l) = x) then true
                else findel(x,tl l)
        fun getind(x, l, i) = 
            if(hd(l)) = x then i
            else getind(x, tl l, i+1)
        fun trimzero(xs: int list) = 
            if null(xs) then [] 
            else
                if hd(xs) = 0 then 
                    trimzero(tl xs) 
                else xs
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
        fun subtractLists(bigs: int list, smalls: int list, borrow: int) =
            if(null(bigs)) then []
            else if null(smalls) then
                if(hd(bigs) + borrow >=0) then (hd(bigs)+borrow)::subtractLists(tl bigs, smalls, 0)
                else (hd(bigs) + borrow + 10)::subtractLists(tl bigs, smalls, ~1)
            else
                    
                if(hd(bigs) -hd(smalls) + borrow >=0) then (hd(bigs)-hd(smalls)+borrow)::subtractLists(tl bigs, tl smalls, 0)
                else (hd(bigs) -hd(smalls) + borrow + 10)::subtractLists(tl bigs, tl smalls, ~1)
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
        fun multBy(xs: int list, y: int, carry: int) =
            if null(xs) then 
                if carry=0 then [] else [carry]
            else 
                (hd(xs)*y + carry) mod 10 :: multBy(tl(xs), y, (hd(xs)*y + carry) div 10)
        fun findDigit(xs: int list, num: int, dividend: int list) =
            let
                val sublist = trimzero(rev(multBy(xs,num, 0)))
            in 	
                if cmp(sublist, dividend) then (num,sublist)
                else findDigit(xs, num-1,dividend)
            end
        fun multiplyHelper(l1, l2) = 
            if (null(l1)) then [] 
            else 
                if (null(l2)) then []
                else 
                    if (null(tl(l2))) then 
                        multBy(rev(l1), hd(l2), 0)
                    else 
                        let val lfur = multiplyHelper(l1, tl(l2))
                        in
                            addLists(0::lfur,multBy(rev(l1), hd(l2), 0), 0)
                        end
        fun divideHelper(curr_div, l2, l1) =
                    if (null(l1)) then 
                        let 
                            val (num, sublist) = findDigit(rev(l2), 9, curr_div)
                        in 
                            ([num], subtractLists(rev(curr_div), rev(sublist), 0))
                        end
                    else 
                        let
                            val (num,sublist) = findDigit(rev(l2), 9, curr_div)
                            val (quotient, remainder) = divideHelper(trimzero(rev(hd(l1)::subtractLists(rev(curr_div), rev(sublist), 0))), l2, tl(l1))
                        in
                            (num::quotient, remainder)
                        end
        fun calcRecPart((l1,s1), (l2,s2), xs, quotlist) = 
                let 
                    val (quotient, remainder) = divideHelper(l1, l2, [0])
                    val quo = trimzero(quotient)@[0]
                    val rem = trimzero(rev(remainder))
                    val nqlist = rev(hd(quo)::rev(quotlist))
                in
                    if(null(rem)) then 
                        (listToString(trimzero(nqlist)), listToString([0]))
                    else 
                        if(findel(rem, xs)) then 
                        let 
                            val x = getind(rem, xs, 0)
                            
                            val nonrec = listpreindex(nqlist, x)
                            val recu = listpostindex(nqlist, x)
                        in
                            (listToString((nonrec)), listToString(recu))
                        end
                        else 
                            if(null quo) then
                                calcRecPart((rem, s1), (l2, s2), rev(rem::rev(xs)), rev(0::rev(quotlist)))
                            else 
                                calcRecPart((rem, s1), (l2, s2), rev(rem::rev(xs)), rev(hd(quo)::rev(quotlist)))
                end 
        (*End of helper functions*)
        (*Functions for bigint*)
        type bigint = int list * int
        val repZero : bigint = ([0],1)
        fun extlist((l,s)) = l
        fun toBigint s = 
            if s = "" then ([], 1) 
            else 
                if String.sub(s,0) = #"~" then 
                    (trimzero(map (fn x => if(ord x - ord #"0")>=0 andalso (ord x - ord #"0")<10 then (ord x - ord #"0") else raise InvalidIntegerInput) (explode (String.extract (s, 1, NONE)))), ~1) 
                else 
                    (trimzero(map (fn x => if(ord x - ord #"0")>=0 andalso (ord x - ord #"0")<10 then (ord x - ord #"0") else raise InvalidIntegerInput) (explode s)), 1)
        fun revSign ((l, s)) = 
            (l, s * ~1)
        fun addBigint ((l1, s1), (l2,s2)) = 
            if s1 = s2 then 
                (rev(addLists(rev(l1), rev(l2), 0)), s1) 
            else 
                if cmp(l1, l2) then 
                    (rev(subtractLists(rev(l2), rev(l1), 0)), s2) 
                else 
                    (rev(subtractLists(rev(l1), rev(l2), 0)), s1)
        fun subtractBigint ((l1, s1), (l2,s2)) = 
            if s1 = ~s2 then 
                (rev(addLists(rev(trimzero(l1)), rev(trimzero(l2)), 0)), s1) 
            else 
                if cmp(trimzero(l1), trimzero(l2)) then 
                    (rev(subtractLists(rev(trimzero(l2)), rev(trimzero(l1)), 0)), ~s2) 
                else 
                    (rev(subtractLists(rev(trimzero(l1)), rev(trimzero(l2)), 0)), s1)
        fun multiplyBigint ((l1, s1), (l2,s2)) = 
            if s1 = s2 then 
                (rev(multiplyHelper(l1, rev(l2))), 1) 
            else 
                (rev(multiplyHelper(l1, rev(l2))), ~1)
        fun divideBigint((l1, s1), (l2, s2)) = 
            if null(trimzero(l2)) then raise ZeroDivision
            else 
            if s1 = s2 then 
                (trimzero(#1 (divideHelper([], l2, l1))), 1) 
            else 
                (trimzero(#1 (divideHelper([], l2, l1))), ~1)
                    
        fun moduloBigint((l1,s1), (l2,s2)) = 
            if s1 = s2 then 
                (trimzero(rev(#2 (divideHelper([], l2, l1)))), 1) 
            else 
                subtractBigint((l1,s1), multiplyBigint(divideBigint((l1,s1), (l2,s2)), (l2,s2)))
        fun power10Bigint(x, list) = 
            if(x < 0) then raise NegativePower
            else 
                if(x=0) then (1::list, 1)
                else power10Bigint(x-1, 0::list)
        fun showBigint (l, s) =
            if(null (trimzero(l))) then "0"
            else 
                if s = ~1 then 
                    "~" ^ implode (map (fn s => String.sub(s, 0)) (map Int.toString (trimzero(l)))) 
                else 
                    implode (map (fn s => String.sub(s, 0)) (map Int.toString (trimzero(l))))
        fun equalBigint ((l1, s1), (l2, s2)) = 
            if(trimzero(l1) = trimzero(l2) andalso null(trimzero(l1))) then true
            else 
                if s1 = s2 then
                    if length(trimzero(l1)) = length(trimzero(l2)) then 
                        if null(l1) orelse null(l2) then true
                        else 
                            if hd(l1) = hd(l2) then 
                                equalBigint((tl l1, s1), (tl l2, s2)) 
                            else false
                    else false
                else false
        fun greaterBigint((l1, s1), (l2, s2)) = 
            if s1 = s2 then 
                if s1 = 1 then 
                    if length(l1) > length(l2) then true
                    else if length(l1) < length(l2) then false
                    else 
                        if null(l1) then false
                        else 
                            if hd(l1) > hd(l2) then true
                            else if hd(l1) < hd(l2) then false
                            else greaterBigint((tl l1, s1), (tl l2, s2))
                else 
                    if length(l1) > length(l2) then false
                    else if length(l1) < length(l2) then true
                    else 
                        if null(l1) then false
                        else 
                            if hd(l1) > hd(l2) then false
                            else if hd(l1) < hd(l2) then true
                            else greaterBigint((tl l1, s1), (tl l2, s2))
            else if s1 = ~1 then false
            else true
        fun lessBigint((l1, s1), (l2, s2)) =
            if s1 = s2 then 
                if s1 = 1 then 
                    if length(l1) > length(l2) then false
                    else    
                        if length(l1) < length(l2) then true
                        else 
                        if null(l1) then false
                        else 
                            if hd(l1) > hd(l2) then false
                            else if hd(l1) < hd(l2) then true
                            else lessBigint((tl l1, s1), (tl l2, s2))   
                else 
                    if length(l1) > length(l2) then true
                    else if length(l1) < length(l2) then false
                    else 
                        if null(l1) then false
                        else 
                            if hd(l1) > hd(l2) then true
                            else if hd(l1) < hd(l2) then false
                            else lessBigint((tl l1, s1), (tl l2, s2))
            else if s1 = ~1 then true
            else false    
        fun calcGCD((l1,s1), (l2,s2)) = 
            if (null(trimzero(l1)) andalso null(trimzero(l2))) then  ([],1)
            else    
                if(null(trimzero(l1))) then (trimzero(l2), 1)
                else    
                    if(null(trimzero(l2))) then (trimzero(l1), 1)
                    else    
                        if (moduloBigint((l1, s1), (l2, s2)) = ([], 1)) then (trimzero(l2), 1)
                        else if (moduloBigint((l2, s2), (l1, s1)) = ([], 1)) then (trimzero(l1), 1)
                        else calcGCD((l2, s2), (moduloBigint((l1, s1), (l2, s2))))

    end
signature RATIONAL =
        sig
        type rational
        type bigint
        exception rat_error
        exception NoDecimalPoint
        val make_rat: bigint * bigint -> rational option
        val rat: bigint -> rational option
        val reci: bigint -> rational option
        val neg: rational -> rational
        val inverse : rational -> rational option
        val equal : rational * rational -> bool (* equality *)
        val less : rational * rational -> bool (* less than *)
        val add : rational * rational -> rational (* addition *)
        val subtract : rational * rational -> rational (* subtraction *)
        val multiply : rational * rational -> rational (* multiplication *)
        val divide : rational * rational -> rational option (* division *)
        val showRat : rational -> string
        val showDecimal : rational -> string
        val toDecimal : rational -> string
        val fromDecimal: string -> rational
        val fromInteger: string -> rational
        end
functor RATIONAL(BigInt: BIGINT) : RATIONAL =
    struct
        type bigint = BigInt.bigint
        type rational = BigInt.bigint * BigInt.bigint
        exception rat_error;
        exception NoDecimalPoint;
        fun trimzero(xs: int list) = 
            if null(xs) then [] 
            else
                if hd(xs) = 0 then 
                    trimzero(tl xs) 
                else xs
        fun getIndex(string, substring, depth) = 
            if (size(string) < size(substring)) then ~1
            else
                if(String.isPrefix substring string) then depth
                else getIndex(String.extract(string, 1, NONE), substring, depth+1)      
        fun make_rat (n, d) = 
            let 
                val gcd = BigInt.calcGCD(n, d)
            in
                if BigInt.equalBigint(d, (BigInt.toBigint "0")) then NONE
                else 
                    if BigInt.equalBigint(n, (BigInt.toBigint "0")) then SOME((BigInt.toBigint "0"), (BigInt.toBigint "1"))
                    else 
                        if BigInt.greaterBigint(d, (BigInt.toBigint "0")) then SOME(BigInt.divideBigint(n, gcd), BigInt.divideBigint(d, gcd))
                        else SOME(BigInt.divideBigint(BigInt.revSign(n), gcd), BigInt.divideBigint(BigInt.revSign(d), gcd))
            end
        fun rat n = make_rat(n, (BigInt.toBigint "1"));
        fun reci n = make_rat((BigInt.toBigint "1"), n);
        fun neg (n, d) = valOf(make_rat(BigInt.revSign(n), d));
        fun inverse ((n, d)) = make_rat(d, n)
        fun equal ((n1, d1), (n2, d2)) = 
            if BigInt.equalBigint(BigInt.multiplyBigint(n1, d2), BigInt.multiplyBigint(n2, d1)) then true
            else false
        fun less (x, y) = 
            let
                val (n1, d1) = valOf(make_rat(x));
                val (n2, d2) = valOf(make_rat(y));
            in            
            if BigInt.lessBigint(BigInt.multiplyBigint(n1, d2), BigInt.multiplyBigint(n2, d1)) then true
            else false
            end
        fun add ((n1, d1), (n2, d2)) = 
            case make_rat(BigInt.addBigint(BigInt.multiplyBigint(n1, d2), BigInt.multiplyBigint(n2, d1)), BigInt.multiplyBigint(d1, d2)) of
                NONE => raise rat_error
                | SOME(r) => r
        fun subtract ((n1, d1), (n2, d2)) = 
            case make_rat(BigInt.subtractBigint(BigInt.multiplyBigint(n1, d2), BigInt.multiplyBigint(n2, d1)), BigInt.multiplyBigint(d1, d2)) of
                NONE => raise rat_error
                | SOME(r) => r
        fun multiply ((n1, d1), (n2, d2)) =
            case make_rat(BigInt.multiplyBigint(n1, n2), BigInt.multiplyBigint(d1, d2)) of
                NONE => raise rat_error
                | SOME(r) => r
        fun divide ((n1, d1), (n2, d2)) =
            case make_rat(BigInt.multiplyBigint(n1, d2), BigInt.multiplyBigint(d1, n2)) of
                NONE => raise rat_error
                | SOME(r) => SOME(r)
        fun showRat ((n, d)) =
            let
                val (n1,d1) = valOf(make_rat(n,d))
            in
                if(null(trimzero(#1 n1))) then 
                    BigInt.showBigint (BigInt.repZero) ^ "/" ^ BigInt.showBigint d1
                else
                    BigInt.showBigint n1 ^ "/" ^ BigInt.showBigint d1 
            end
        fun showDecimal ((n, d)) =
            case make_rat(n,d) of NONE => raise BigInt.ZeroDivision |
                SOME(n1,d1) => 
                let
                    val main = BigInt.divideBigint (n1, d1)
                    val rem = BigInt.moduloBigint (n1, d1)
                    val (nonrecPart, recPart) = BigInt.calcRecPart(rem, d1, [BigInt.extlist(rem)], [])
                in
                    if(null(trimzero(#1 main))) then 
                            BigInt.showBigint (([0], 1)) ^ "." ^ nonrecPart ^ "(" ^ recPart ^ ")"
                        else 
                            BigInt.showBigint main ^ "." ^ nonrecPart ^ "(" ^ recPart ^ ")"
                end
        fun toDecimal(n,d)  = showDecimal(n,d)
        fun parseRat(s: string)= 
            if(String.sub(s,0)= #"~") then 
                let 
                    val decpoint = getIndex(s, ".", 0)
                in 
                    if(decpoint = ~1) then raise NoDecimalPoint
                    else
                        let
                            val num=String.extract(s, 1, SOME(valOf(SOME(decpoint))-1))
                            val nonrecPart=String.extract(s, decpoint+1,    SOME(getIndex(s, "(", 0)-decpoint-1))
                            val recPart=String.extract(s, getIndex(s, "(", 0)+1, SOME(getIndex(s, ")", 0)-getIndex(s, "(", 0)-1))
                            val num1=BigInt.toBigint(num^nonrecPart^recPart)
                            val num2=BigInt.toBigint(num^nonrecPart)
                        in
                            neg(valOf(make_rat(BigInt.subtractBigint(num1, num2),BigInt.subtractBigint(BigInt.power10Bigint(size(recPart) + size(nonrecPart), []), BigInt.power10Bigint(size(nonrecPart), [])) )))
                        end
                end
            else
                if(String.sub(s,0) = #"+") then 
                    let 
                        val decpoint=getIndex(s, ".", 0)
                    in 
                        if(decpoint = ~1) then raise NoDecimalPoint
                        else
                            let
                                val num=String.extract(s, 1, SOME(valOf(SOME(decpoint))-1))
                                val nonrecPart=String.extract(s, decpoint+1,    SOME(getIndex(s, "(", 0)-decpoint-1))
                                val recPart=String.extract(s, getIndex(s, "(", 0)+1, SOME(getIndex(s, ")", 0)-getIndex(s, "(", 0)-1))
                                val num1=BigInt.toBigint(num^nonrecPart^recPart)
                                val num2=BigInt.toBigint(num^nonrecPart)
                            in
                            valOf(make_rat(BigInt.subtractBigint(num1, num2),BigInt.subtractBigint(BigInt.power10Bigint(size(recPart) + size(nonrecPart), []), BigInt.power10Bigint(size(nonrecPart), [])) ))
                            end
                    end
                else
                    let 
                        val decpoint=getIndex(s, ".", 0)
                    in 
                        if(decpoint = ~1) then raise NoDecimalPoint
                        else
                            let
                                val num=String.extract(s, 0, SOME(decpoint))
                                val nonrecPart=String.extract(s, decpoint+1,    SOME(getIndex(s, "(", 0)-decpoint-1))
                                val recPart=String.extract(s, getIndex(s, "(", 0)+1, SOME(getIndex(s, ")", 0)-getIndex(s, "(", 0)-1))
                                val num1=BigInt.toBigint(num^nonrecPart^recPart)
                                val num2=BigInt.toBigint(num^nonrecPart)
                            in
                                valOf(make_rat(BigInt.subtractBigint(num1, num2),BigInt.subtractBigint(BigInt.power10Bigint(size(recPart) + size(nonrecPart), []), BigInt.power10Bigint(size(nonrecPart), [])) ))
                            end
                    end
        fun fromDecimal(s: string) = parseRat(s)
        fun fromInteger(s: string) = valOf(rat(BigInt.toBigint(s)))
    end
structure ExpOp = RATIONAL(BigInt)
