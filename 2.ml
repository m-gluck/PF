(* Błędy OCamla są STRASZNE, ŻE AŻ RĘCE OPADAJĄ!!! *)
(* Już mi trochę lepiej :-) *)

(* 1. *)
let rec sublists l = match l with
	| (x::xs) -> let rec h pending acc = match pending with
		| (y::ys) -> h ys ( (x::y)::acc )
		| [] -> acc
		in
			let ss = sublists xs in h ss ss
	| [] -> [[]]
;;
sublists [1;2;3] ;;
(* 2. *)
let cycle l n =
	let rec h l beg_len acc =
		if beg_len=0 then
			l @ (List.rev acc)
		else
			h (List.tl l) (beg_len-1) ( List.hd l :: acc )
	in h l (List.length l - n) []
;;
cycle [1; 2; 3; 4] 3 ;;
(* 3.1. *)
let rec merge cmp l1 l2 =
	match (l1, l2) with
	| (x::xs, y::ys) ->
		if cmp x y then
			x::merge cmp xs l2
		else
			y::merge cmp l1 ys
	| ([], _) -> l2
	| (_, []) -> l1
;;
merge (<=) [1; 2; 5] [3; 4; 5] ;;
(* 3.2. *)
let rec merge_iter ?(acc=[]) cmp l1 l2 =
	match (l1, l2) with
	| (x::xs, y::ys) ->
		if cmp x y then
			merge_iter ~acc:(x::acc) cmp xs l2
		else
			merge_iter ~acc:(y::acc) cmp l1 ys
	| ([], _) -> List.rev_append acc l2
	| (_, []) -> List.rev_append acc l1
;;
(*
Obie wersje mają taką samą złożoność pamięciową: O(min{n1, n2}).
Może trochę lepsza jest merge_iter, bo akumulator zajmuje pewnie mniej miejsca niż stos wywołań.
*)
merge_iter (<=) [1; 2; 5] [3; 4; 5] ;;
(* 3.3. *)
let rec mergesort cmp l =
	if l=[] || List.tl l=[] then
		l
	else
		let rec h l acc n =
			if n=0 then
				merge cmp (mergesort cmp l) (mergesort cmp acc)
			else
				h (List.tl l) (List.hd l :: acc) (n-1)
		in h l [] (List.length l / 2)
;;
mergesort (<) [4; 5; 7; 1; 2; -10; 33; -207; 36];;
(* 3.4. *)
(* Ja znam rev_append, ale domyślam się, że autor zadania zakładał użycie rev i append w merge_iter. *)
let rec rev_merge ?(acc=[]) cmp l1 l2 = (* rev_merge l1 l2 = List.rev (merge l1 l2) *)
	match (l1, l2) with
	| (x::xs, y::ys) ->
		if cmp x y then
			rev_merge ~acc:(x::acc) cmp xs l2
		else
			rev_merge ~acc:(y::acc) cmp l1 ys
	| ([], _) -> List.rev_append l2 acc
	| (_, []) -> List.rev_append l1 acc
let rec mergesort2 cmp l =
	if l=[] || List.tl l=[] then
		l
	else
		let rec ncmp x y = not @@ cmp x y
		and h l acc n =
			if n=0 then					
				rev_merge ncmp (mergesort2 ncmp l) (mergesort2 ncmp acc)
			(*
			Scalam porządkniem ~cmp i odwracam listy posortowane ~cmp czyli odwrotnie.
			Nie można zrobić tego cmp, bo wtedy mergesort2 nie zwraca listy posortowanej
			jakimkolwiek porządkiem, bo każdy porządek jest antysymetryczny.
			*)
			else
				h (List.tl l) (List.hd l :: acc) (n-1)
		in h l [] (List.length l / 2)
;;
mergesort2 (<) [4; 5; 7; 1; 2; -10; 33; -207; 36];;
(* 4. *)
let rec insert a n l = (* wstawia a do l na pozycję n (licząc od 0) *)
	match (n, l) with
	| (0, _) -> a::l
	| (_, x::xs) -> x::insert a (n-1) xs
	| _ -> failwith @@ "insert: invalid argument n="^string_of_int n^"; 0 <= n <= List.length l"
let perms l =
	let rec go len l =
		match l with
		| x::xs -> List.concat @@
			List.rev_map
				(let rec h acc n l =
					if n = -1 then
						acc
					else
						h (insert x n l::acc) (n-1) l
				in h [] (len-1))
				(go (len-1) xs)
		| [] -> [[]]
	in go (List.length l) l
;;
perms [1; 2; 3; 4];;
List.length (perms [-5; -6; -7; 4; 111; 1024]) = 720;;
(* 5. *)
let rec suffixes ?(acc=[[]]) l =
	match l with
	| _::xs -> suffixes ~acc:(l::acc) xs
	| [] -> acc
;;
suffixes [1; 2; 3];;
let rec take n l =
	match (n, l) with
	| (0, _) -> []
	| (_, x::xs) -> x::take (n-1) xs
	| _ -> failwith "invalid argument; 0 <= n <= List.length l"
let prefixes l =
	let rec go acc len =
		if len=0 then
			acc
		else
			go (take len l::acc) (len-1)
	in go [[]] (List.length l)
;;
prefixes [1; 2; 3];;

type 'a clist = { clist : 'z. ('a -> 'z -> 'z) -> 'z -> 'z }
(* 6. *)
let cnil = { clist = fun _ s -> s }
let ccons x xs = { clist = fun f s -> f x (xs.clist f s) }
let map g l = { clist = function f -> l.clist (fun x ys -> f (g x) ys) }
let append l1 l2 = { clist = fun f s -> l1.clist f (l2.clist f s) }
let clist_to_list l = l.clist List.cons []
let l1 = ccons 101 cnil
let l2 = ccons 201 @@ ccons 202 cnil
let l3 = ccons 301 @@ ccons 302 @@ ccons 303 cnil
let l4 = ccons 401 @@ ccons 402 @@ ccons 403 @@ ccons 404 cnil
;;
clist_to_list cnil;;
clist_to_list @@ map ((+) 1) @@ append l3 l4;;
let prod l1 l2 = { clist = function f -> l1.clist ( function x -> l2.clist (function y -> f (x, y)) ) } ;;
(*
Funkcja prod dodaje każdy element l1 tyle razy, ile jest elementów w l2.
Za każdym razem z kolejnym elementem l2.z
*)
clist_to_list @@ prod l3 l4;;

let concat l = l.clist append cnil;;
let ll1 = ccons (ccons 1 @@ ccons 2 cnil)
	@@ ccons (ccons 3 cnil)
	@@ ccons (ccons 1 @@ ccons 2 cnil) cnil
;;
clist_to_list (concat ll1);;
let fold_right f l s = l.clist f s;;
clist_to_list @@ fold_right (fun e acc -> ccons e acc) cnil l4;;
(* odpowiednik potęgowania *)
let vars args vals = (* Lista wszystkich funkcji args-->vals w formie list par (argument, wartość). *)
	fold_right
	(fun ar vrs ->
		concat @@
			map
				(fun vr -> map (fun v -> ccons (ar,v) vr) vals)
				vrs
	)
	args
	(ccons cnil cnil)
;;
List.map clist_to_list @@ clist_to_list @@ vars l2 l4;;
(* 7. *)
let ctail l = fst @@
	fold_right (fun n (_, p) -> (p, ccons n p)) l (cnil, cnil)
;;
clist_to_list @@ ctail l4;;
clist_to_list @@ ctail l1;;
clist_to_list @@ ctail cnil
(* 8. *)
type ('a, 'z) sq = { sq : 's. 's -> ('s->'a) -> ('s->'s) -> 'z }
type 'a stream = { stream : 'z. ('a, 'z) sq -> 'z }
let tail s = s.stream { sq = fun s hd tl -> { stream = fun sq -> sq.sq (tl s) hd tl } }
let nat = { stream = fun sq -> sq.sq 0 (fun x -> x) succ }

let unfold f start =
	{ stream = fun sq ->
		let cur s = fst (f s)
		and nxt s = snd (f s)
		in sq.sq start cur nxt
	}
let head s = s.stream { sq = fun s hd _ -> hd s}
let map f s = s.stream { sq = fun s hd tl -> { stream = fun sq -> sq.sq s (fun s -> f @@ hd s) tl } }
;;
head nat;;
head @@ tail @@ tail @@ map (fun n -> n/2) nat;;
let skip_odd s = s.stream { sq = fun s hd tl -> { stream = fun sq -> sq.sq s hd (fun s -> tl @@ tl s) } }
;;
head @@ tail @@ tail @@ skip_odd nat;;
let rec prefix n s =
	if (n=0) then
		[]
	else
		head s::prefix (n-1) (tail s)
;;
prefix 5 @@ skip_odd nat;;
let pnp (* liczby nieparzyste o parzystych numerach zaczynając od 0 *)= unfold (fun n -> (2*n+1, n+2)) 0 ;;
prefix 10 pnp;;
(* 9. *)
let cons x s = s.stream
	{ sq = fun s hd tl ->
	{ stream = fun sq -> sq.sq
		(s, true)
		(function
			| (s, false) -> hd s
			| (s, true) -> x
		)
		(function
			| (s, false) -> (tl s, false)
			| (s, true) -> (s, false)
		)
	}
	}
;;
prefix 4 @@ cons (-1) nat;;
(* 10. *)
let scan f a s = s.stream { sq = fun s hd tl ->
	{ stream = fun sq ->
		sq.sq
			(s, a)
			(function (pos, prev) -> f prev (hd pos))
			(function (pos, prev) -> (tl pos, f prev (hd pos)))
			(* Nie można tych funkcji ponazywać, bo wyskakuje
			jakiś niezrozumiały błąd: "operator expected". *)
	}
}
;;
prefix 6 @@ scan (+) 0 nat = [0; 1; 3; 6; 10; 15];;
