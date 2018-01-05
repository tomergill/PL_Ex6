(*
 * Name:         Tomer Gill
 * I.D.:         318459450
 * Group Number: 89-310-05
 * U2 user name: gilltom
 *)
 
exception IllegalArgumentException;

(******** Auxiliary functions ********)

(*
 * Auxiliary function
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
        end
;
        
        
(*
 * Auxiliary function
 * Filters ls by predicate p
 *)
fun filter p ls = 
    if null ls then []
    else if p(hd ls) then (hd ls)::(filter p (tl ls)) else filter p (tl ls)
;


(*
 * Auxiliary function
 * Reduces the list from unit with oper
 *)
fun reduce (oper, u) ls = 
    let 
        fun reduce_helper (total, i) = 
            if i >= List.length(ls) then total
            else reduce_helper(oper(total, List.nth(ls, i)), i + 1)
    in
        reduce_helper(u, 0)
    end
;
 
 
(************ Part 1 ************)
      
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

datatype Arguments
 = IntPair of int * int
 | RealTriple of real * real * real
 | StringSingle of string;

datatype OutputArgs = IntNum of int | RealNum of real | Str of string;


(*
 * 3
 * multiFunc
 * Output depends on input:
 * --- If got IntPair returns the elements multiplication.
 * --- If got RealTriple returns the average.
 * --- If got StringSingle returns it's reverse.
 * Any ways all the outputs' type is OutputArgs.
 *)
fun multiFunc (IntPair(x, y)) = IntNum(x * y)
|   multiFunc (RealTriple(x, y, z)) = RealNum((x + y + z) / 3.0)
|   multiFunc (StringSingle(s)) = Str(implode(rev(explode(s))))
;


(************ Part 4 ************)

datatype Square = Alive of int | Dead of int;
datatype GameStatus = Extinct | OnGoing;
type LifeState = Square list list * GameStatus;


(*
 * 4.1
 * createLifeGrid
 * Creates a life grid (list of lists of Squares) sized nxn where the cells in lives are of type Alive(0) and those
 * who aren't are Dead(0)
 *)
fun createLifeGrid (n, lives) = if n < 0 then raise IllegalArgumentException else if n = 0 then [] else
    let
        fun isInRow (r:int) ((x,y)) = x=r;
        fun isColIn (c:int) (total, (x,y)) = total orelse (y = c);
        fun createLifeRow (j, alives) = 
            if j >= n then []
            else
                if (reduce ((isColIn j), false) (alives)) (* If this cell is alive *)
                then Alive(0)::createLifeRow(j + 1, alives)
                else Dead(0)::createLifeRow(j + 1, alives)
        ;
        fun createLifeGrid_helper i = 
            if i >= n then []
            else
                let
                    val AliveInRow = (filter (isInRow i) lives)
                    val row = createLifeRow(0, AliveInRow);
                in
                    [row] @ (createLifeGrid_helper (i + 1))
                end
        ;
    in
        createLifeGrid_helper 0
    end
;


(*
 * 4.2
 * determineStatusOf
 * Returns if the game is OnGoing or Extinct
 *)
fun determineStatusOf grid = 
    let
        fun isCellAlive(total, Alive(_)) = true
        |   isCellAlive(total, Dead(_)) = total orelse false
        ;
        fun isRowAlive(total, row) = total orelse (reduce (isCellAlive, false) row);
    in
        if (reduce (isRowAlive, false) grid) then OnGoing else Extinct
    end
;


(*
 * Auxiliary function
 * Returns how many neighbours of (i, j) are alive in grid
 *)
fun howManyAliveNeighbours(grid, i, j) = 
    let 
        val n = List.length(grid);
        fun neighbourIn (x,y) = 
            if x >= 0 andalso x < n andalso y >= 0 andalso y < n
            then case List.nth(List.nth(grid, x), y)
                of Alive(_) => 1
                |  Dead(_) => 0
            else 0
        ;
    in
        neighbourIn(i -1, j - 1) + neighbourIn(i -1, j) + neighbourIn(i -1, j + 1) +
        neighbourIn(i, j - 1) + neighbourIn(i, j + 1) +
        neighbourIn(i + 1, j - 1) + neighbourIn(i + 1, j) + neighbourIn(i + 1, j + 1)
    end
;


(*
 * Auxiliary function
 * Returns the cell (i, j) in the next generation in grid
 *)
fun determineCell (grid, i, j) =
    let
        val neighbours = howManyAliveNeighbours(grid, i, j);
    in
        case List.nth(List.nth(grid, i), j)
            of Dead(n) => if neighbours = 3 then Alive(0) else Dead(n + 1)
            |  Alive(age) => if neighbours < 2 orelse neighbours > 3 then Dead(0) else Alive(age + 1)
    end
;


(*
 * 4.3
 * nextGeneration
 * Returns grid after one generation
 *)
fun nextGeneration grid = 
    let
        val n = List.length(grid);
        fun determineRow i = if i >= n then [] else
            let
                fun determineCells j = 
                    if j >= n then [] 
                    else determineCell(grid, i, j)::determineCells(j + 1)
                    
                ;
                val row = determineCells 0;
            in
                row::(determineRow (i + 1))
            end
        ;
    in
        determineRow 0
    end
;


(*
 * 4.4
 * determineNState
 * Returns the game after N generations. If the game goes Extinct then the the developing stops and it is returned.
 *)
fun determineNState (state:LifeState, n) = 
    if n < 0 then raise IllegalArgumentException
    else if n = 0 then state
    else 
        case (#2 state) 
            of Extinct => state
            | OnGoing =>
                let
                    val new_grid = nextGeneration (#1 state);
                    val status = determineStatusOf(new_grid);
                in
                    determineNState((new_grid, status), (n - 1))
                end
;


(* (use "test.sml") *)