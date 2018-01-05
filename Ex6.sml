(*
 * Name:         Tomer Gill
 * I.D.:         318459450
 * Group Number: 89-310-05
 * U2 user name: gilltom
 *)
 
exception IllegalArgumentException;
 
 
(************ Part 1 ************)

(*
 * Help function
 * Returns the sublist of ls from place s up to e (not included)
 *)
fun sublist ([], s, e) = []
|   sublist (ls, s, e) = 
        let
            val len = List.length(ls);
            fun sublist_help i = 
                if i < s then sublist_help(i + 1)
                else if i < e then List.nth(ls, i) :: sublist_help(i + 1) 
                else []
            ;
        in
            if s < 0 orelse e > len orelse s >= e
            then raise IllegalArgumentException
            else sublist_help s
        end;

        
(*
 * 1.1
 * rotate
 * Returns ls rotated to the left n times
 *)
fun rotate ([], n) = 
    if (n<0) 
    then raise IllegalArgumentException 
    else []
|   rotate (ls, n) = 
        if (n<0) then raise IllegalArgumentException 
        else if (n = 0) then ls
        else
            let
                val len = List.length(ls)
                val m = n mod len
            in
                sublist(ls, m, len) @ sublist(ls, 0, m)
            end;
 
 
(*
 * 1.2
 * split
 * Returns 2 lists from the received list, for every 2 items one goes to the 1st list and the other to the 2nd.
 *)
fun split [] = ([], [])
|   split (h::[]) = ([h], [])
|   split (h1::h2::rest) = 
        let
            val (l1, l2) = split(rest)
        in
            (h1::l1, h2::l2)
        end
;
        

(*
 * 1.3
 * merge
 * Returns a sorted merged list from 2 sorted lists
 *)        
fun merge (ls, []) = ls
|   merge ([], ls) = ls
|   merge (h1::rest1, h2::rest2) = 
        if h1 < h2 
        then h1::merge(rest1, h2::rest2) 
        else h2::merge(h1::rest1, rest2)
;


(*
 * 1.4
 * sort
 * Returns ls sorted by MergeSort.
 *)
fun sort [] = []
|   sort (h::[]) = [h]
|   sort ls = 
    let
        val (l1, l2) = split ls
        val s1 = sort l1
        val s2 = sort l2
    in
        merge(s1, s2)
    end
;


(************ Part 2 ************)

(*
 * Help function - Filters ls by predicate p
 *)
fun filter p ls = 
    if null ls then []
    else if p(hd ls) then (hd ls)::(filter p (tl ls)) else filter p (tl ls)
;


(*
 * 2.1
 * choose
 * Returns all the possible ways to get k elements from ls
 *)
fun choose (k, ls) = 
    if k < 0 then raise IllegalArgumentException
    else if k = 0 then [[]]
    else if null ls orelse k > List.length(ls) then []
    else 
        let
            fun addToList n ls = n::ls;
            val (h::rest) = ls;
            val chosen = map (addToList h) (choose(k - 1, rest));
            val not_chosen = choose(k, rest);
        in
            filter (fn l => not(null l))  (chosen @ not_chosen)
        end
;


(*
 * 2.2
 * isPolindrom
 * Returns true if str is a palindrome, otherwise false.
 *)
fun isPolindrom str = (str=implode(rev(explode(str))));


(************ Part 3 ************)


(* (use "test.sml") *)