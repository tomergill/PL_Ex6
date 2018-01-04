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


(* (use "test.sml") *)