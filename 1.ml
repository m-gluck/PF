(* 1. *)
function (x:int) -> x;;
fun f g x -> f (g x);;
fun x y -> x;;
fun (x:'a) (y:'a) -> x;;
(* 2. *)
let rec f x = f x
(* 3. *)
let hd (s:int->int) = s 0
let tl (s:int->int) n = s (n+1)
let add (s:int->int) c n = (s n) + c
let map f (s:int->int) n = f (s n)
let map2 f (s1:int->int) (s2:int->int) n = f (s1 n) (s2 n)
let replace a m (s:int->int) n = if n mod m = 0 then a else s n
let take m (s:int->int) n = s (m * n)
let rec scan f (s:int->int) a n = if n=0 then a
	else
	scan f s (f a (s (n-1))) (n-1)
let rec tabulate (s:int->int) n m = if n>m then [] else s n::tabulate s (n+1) m

let sqrs n = n*n
let rec fibs ?(c=0) ?(nxt=1) n = if n=0 then c
	else fibs ~c:nxt ~nxt:(c+nxt) (n-1)
let evens n = 2*n
;;
hd fibs = 0;;
tl sqrs 2 = 9;;
add evens 1 4 = 9;;
map (fun n -> n/2) evens 4 = 4;;
map2 (/) sqrs evens 2 = 1;;
replace (-1) 4 fibs 8 = (-1);;
replace (-1) 4 fibs 6 = 8;;
take 3 sqrs 1 = 9;;
scan (+) evens 1 5 = 21;;
tabulate sqrs 5 5 = [25];;
tabulate sqrs 3 7 = [9;16;25;36;49];;
(* 4. *)
let ctrue (x:'a) (y:'a) = x
let cfalse (x:'a) (y:'a) = y
let cand (f:'a->'a->'a) (g:'a->'a->'a) x y = f (g x y) y
let cor (f:'a->'a->'a) (g:'a->'a->'a) x y = f x (g x y)
let cbool_of_bool b = fun (x:'a) (y:'a) -> if b then x else y
let bool_of_cbool f = f true false
(* 5. *)
let zero (f:'a->'a) (x:'a) = x
let succ l (f:'a->'a) (x:'a) = f (l f x)
let add (l1:('a->'a)->'a->'a) (l2:('a->'a)->'a->'a) f x = l2 f (l1 f x)
let mul (l1:('a->'a)->'a->'a) l2 (f:'a->'a) x = l1 (l2 f) x
let is_zero (l:('a->'a)->'a->'a) (x:'a) y = if l==zero then x else y
let rec cnum_of_int n f x = if n=0 then x else f (cnum_of_int (n-1) f x)
let int_of_cnum (l:(int->int)->int->int) = l ((+) 1) 0
(* 6. *)
type cbool = { cbool : 'a. 'a -> 'a -> 'a }
type cnum = {cnum : 'a. ('a -> 'a) -> 'a -> 'a}

let ctrue = { cbool = fun x y -> x }
let cfalse = { cbool = fun x y -> y }
let cand f g = { cbool = fun x y -> f.cbool (g.cbool x y) y }
let cor f g = { cbool = fun x y -> f.cbool x (g.cbool x y) }
let cbool_of_bool b = { cbool = fun x y -> if b then x else y }
let bool_of_cbool f = f.cbool true false

let zero = { cnum = fun _ x -> x }
let succ l = { cnum = fun f x -> f (l.cnum f x) }
let add l1 l2 = {cnum = fun f x -> l2.cnum f (l1.cnum f x) }
let mul l1 l2 = { cnum = fun f x -> l1.cnum (l2.cnum f) x }
let is_zero l = { cbool = fun x y -> if l==zero then x else y }
let rec cnum_of_int n = {cnum = fun f x -> if n=0 then x else f ((cnum_of_int (n-1)).cnum f x) }
let int_of_cnum l = l.cnum ((+) 1) 0
