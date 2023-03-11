signature RATIONAL =
    sig
        type rational
        exception rat_error
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
        val fromDecimal : string -> rational
        val toDecimal : rational -> string
    end;

signature BIGINT = 
    sig
        type bigint
        val toBigint: string -> bigint
        val revSign: bigint -> bigint
        val addBigint  : bigint * bigint -> bigint
        val subtractBigint  : bigint * bigint -> bigint
        val multiplyBigint : bigint * bigint -> bigint
        val equalBigint : bigint * bigint -> bool
        val lessBigint : bigint * bigint -> bool
        val greaterBigint : bigint * bigint -> bool
        val showBigint : bigint -> string
    end

structure BigInt : BIGINT =
    struct 
        (*Helper functions*)
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

        (*End of helper functions*)
        (*Functions for bigint*)
        type bigint = int list * int
        fun toBigint s = 
            if s = "" then ([], 1) 
            else 
                if String.sub(s,0) = #"~" then 
                    (map (fn x => ord x - ord #"0") (explode (String.extract (s, 1, NONE))), ~1) 
                else 
                    (map (fn x => ord x - ord #"0") (explode s), 1)
        fun revSign (l, s) = 
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
                (rev(addLists(rev(l1), rev(l2), 0)), s1) 
            else 
                if cmp(l1, l2) then 
                    (rev(subtractLists(rev(l2), rev(l1), 0)), ~s2) 
                else 
                    (rev(subtractLists(rev(l1), rev(l2), 0)), s1)
        fun multiplyBigint ((l1, s1), (l2,s2)) = 
            if s1 = s2 then 
                (rev(multiplyHelper(l1, rev(l2))), 1) 
            else 
                (rev(multiplyHelper(l1, rev(l2))), ~1)
        fun divideBigInt((l1, s1), (l2, s2)) = 
        fun showBigint (l, s) =
            if s = ~1 then 
                "~" ^ implode (map (fn s => String.sub(s, 0)) (map Int.toString l)) 
            else 
                implode (map (fn s => String.sub(s, 0)) (map Int.toString l))
        fun equalBigint ((l1, s1), (l2, s2)) = 
            if s1 = s2 then 
                if length(l1) = length(l2) then 
                    if null(l1) then true
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
                    else if length(l1) < length(l2) then true
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
    end
