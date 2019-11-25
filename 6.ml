(* 1. *)
let rec fix f x = f (fix f) x

let rec fix_with_limit limit f x =
	if limit = 0 then
		failwith "The recursion is too deep!"
	else
		f (fix_with_limit (limit - 1) f) x

let rec fix_memo =
	let memtbl = Hashtbl.create 50 in
		fun f x ->
			match Hashtbl.find_opt memtbl x with
			| Some v -> v
			| None ->
				let computed_val = f (fix_memo f) x in
					begin
						Hashtbl.add memtbl x computed_val;
						computed_val
					end
let fib_f fib n =
	begin
		print_string "Wywołanie fib_f z n=";
		print_int n;
		print_char '\n';
	if n <= 1 then n
	else fib (n-1) + fib (n-2)
	end
let fib = fix fib_f
;;
fib 7;;
(* (fix_with_limit 6 fib_f) 7;; (* To zakończy program. *) *)
(fix_with_limit 7 fib_f) 7;;
(fix_memo fib_f) 7;;
(fix_memo fib_f) 7;;
(fix_memo fib_f) 8;;
(* 2. *)
let fix f x =
	let specialised_fix = ref (fix f) in
		f !specialised_fix x
;;
(fix fib_f) 7;;
(* 3. *)
let (next, reset) =
	let cnt = ref 0 in
		(
			(
			function () ->
				let r = !cnt in
				begin
					cnt := !cnt + 1;
					r
				end
			)
			,
			(function () -> cnt := 0)
		)
;;
next();;
next();;
next();;
reset();;
next();;
next();;
(* 4. *)
type 'a infseq = int -> 'a;;
let pi4 n = (if n mod 2 = 0 then (~+.) else (~-.)) (1.0 /. float_of_int (2*n + 1))
let rec partial_sum ?(acc=0.0) seq ?(begin_at=0) end_at =
	if end_at < begin_at then
		acc
	else
		partial_sum ~acc:(acc +. seq begin_at) seq ~begin_at:(begin_at + 1) end_at
let group3 f seq n = f (seq @@ n) (seq @@ n+1) (seq @@ n+2)
let euler_transform x y z = z -. ( (y-.z)**2.0 /. (x -. 2.0*.y +. z) )
let fast_pi4 = group3 euler_transform (partial_sum pi4)
;;
4.0 *. partial_sum pi4 63;;
4.0 *. fast_pi4 20;; (* Tyle samo obliczeń. *)
(* 5. *)
type 'a dllist = 'a dllist_data Lazy.t
(* Tu musi być Lazy.t zamiast lazy_t. Tak jest w dokumentacji i inaczej nie działa. *)
and 'a dllist_data = { prev:'a dllist ; elem:'a ; next:'a dllist }

let prev (lazy dl_d) = dl_d.prev
let next (lazy dl_d) = dl_d.next
let elem (lazy dl_d) = dl_d.elem
let mk_dl e =
	let rec x = lazy {prev=x ; elem=e ; next=x} in
		x
let dl1 = mk_dl 2
let dl2 =
	let rec	x = {prev=lazy z ; elem='x' ; next=lazy y} and
		y = {prev=lazy x ; elem='y' ; next=lazy z} and
		z = {prev=lazy y ; elem='z' ; next=lazy x} in
			lazy x
(* Punkt wejścia do listy to jej pierwszy element. *)
let of_list l =
	let	es = Array.of_list l in
	let	len = Array.length es in
	let	a = Array.make len (mk_dl es.(0)) in
	let	mk_block p c n = lazy {prev=a.(p) ; elem=es.(c) ; next=a.(n)} and
		i = ref 1 in
	begin
		a.(0) <- mk_block (len-1) 0 1;
		a.(len-1) <- mk_block (len-2) (len-1) 0;
		while !i < len-1 do
			a.(!i) <- mk_block (!i - 1) !i (!i + 1);
			i := !i + 1
		done;
		a.(0)
	end

let dl3 = of_list ['a' ; 'b' ; 'c']
;;
dl2;;
elem dl2;;
elem @@ prev dl2;;
elem @@ next dl2;;
dl2 == prev (next dl2);;
dl2 == next (prev dl2);;

dl3;;
elem dl3;;
elem @@ prev dl3;;
elem @@ next dl3;;
dl3 == prev (next dl3);;
dl3 == next (prev dl3);;
(* 6. *)
let integers : int dllist =
	let rec h =
	let tbl = Hashtbl.create 20 in
		function n -> match Hashtbl.find_opt tbl n with
		| Some block -> block
		| None -> let new_block =
			{prev=lazy (h @@ n-1); elem=n ; next=lazy (h @@ n+1)}
			in
			begin
				Hashtbl.add tbl n new_block;
				new_block
			end
	in lazy (h 0)
;;
integers;;
elem integers;;
elem @@ next integers;;
elem @@ prev integers;;
elem @@ next @@ next integers;;
elem @@ prev @@ next integers;;
integers == prev (next integers);;
integers == next (prev integers);;
(*	Tak naprawdę lista się nie rozwarstwia, czego dowodzą poniższe równości.
	To tylko moduł Lazy nie jest w stanie rozpoznać, że
	prev (next integers) == lazy (h 0)
	por.: let f() = () in lazy (f()) == lazy (f());; *)
Lazy.force integers == Lazy.force @@ prev (next integers);;
Lazy.force integers == Lazy.force @@ next (prev integers);;
(* 7. *)
type 'a node = {name : string ; mutable data : 'a ; next : 'a node list Lazy.t}
type state = Clear | Start | Next of state node

let st = Stack.create()
let next_cycle() =
	let res = ref [] in
	begin
	while not(Stack.is_empty st) && !res=[] do
		let v = Stack.pop st in
		List.iter (
			function ch -> match ch.data with
				| Clear ->
					begin
					ch.data <- Next v;
					Stack.push ch st
					end
				| Start ->
					let cur = ref v in
					begin
					while !cur.data <> Start do
						res := !cur.name::!res;
						match !cur.data with
						| Next nxt -> cur := nxt
						| _ -> failwith "internal error!"
					done;
					res := !cur.name::!res;
					end
				| Next _ -> ()
		) (Lazy.force v.next)
	done;
	!res
	end
(* 8. *)
let moves n=
let rec move=
let tbl = Hashtbl.create 100 in
	function (x, y) as args ->
	match Hashtbl.find_opt tbl args with
	| None ->
		let new_v =
		{name= "("^string_of_int x^", "^string_of_int y^")";
		data=Clear;
		next= lazy (
			if x<0 || y<0 || x>=n || y>=n then
				[]
			else
				[	move(x-2, y-1); move(x-2, y+1);
					move(x+2, y-1); move(x+2, y+1);
					move(x-1, y-2); move(x-1, y+2);
					move(x+1, y-2); move(x+1, y+2)
				]
			)
		} in
		begin
			Hashtbl.add tbl args new_v;
			new_v
		end
| Some v -> v
in move(3, 4)
;;
let g = moves 10;;
g.data <- Start;;
Stack.push g st;;
(* 9. *)
type 'a my_lazy = unit -> 'a
let force x = x()
let fix f =
	let	checker() = failwith "Some infinite forcing going on here!" in
	let rec	computation() =
			begin
			f checker;
			f computation
			end
	in computation
;;
