
(* Name: Connor Fong

   UID: 303991911

   Others With Whom I Discussed Things:

   Other Resources I Consulted:
   
*)

exception Not_found

(* Problem 1a
   map2: ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
*)

let rec map2 f l1 l2 : 'a list = 
	match l1 with
		[] -> []
	|	x::xs -> match l2 with
					[] -> []
				|	[y] -> [f x y]
				|	y::ys -> (f x y)::(map2 f xs ys)


(* Problem 1b
   rev: 'a list -> 'a list
*)

let rev l =
	List.fold_right (fun x y -> y@[x]) l []


(* Problem 1c
   rev2: 'a list -> 'a list
*)

let rev2 l = 
	List.fold_left (fun x y -> y::x) [] l


(* Problem 1d
   curry: ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
   uncurry: ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
*)

let curry f =
	fun x -> fun y -> f(x,y)
	
let uncurry f =
	fun (x,y) -> f x y


(* Problem 1e
   mapAllPairs: ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
*)

let mapAllPairs f l1 l2 = 
	List.map (uncurry f) (List.concat (List.map (fun x -> List.map (fun y -> (x,y)) l2) l1))


(* Dictionaries *)    

(* Problem 2a
   empty1: unit -> ('a * 'b) list
   put1: 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
   get1: 'a -> ('a * 'b) list -> 'b
*)  

let empty1 x =
	[]
	
let put1 k v l =
	(k, v)::l
		
let rec get1 k l =
	match l with
		[] -> raise Not_found
	| 	(k', v')::rest -> if k = k' then v'
							else (get1 k rest)
	
(* Problem 2b
   empty2: unit -> ('a,'b) dict2
   put2: 'a -> 'b -> ('a,'b) dict2 -> ('a,'b) dict2
   get2: 'a -> ('a,'b) dict2 -> 'b
*)  
    
type ('a,'b) dict2 = Empty | Entry of 'a * 'b * ('a,'b) dict2

let empty2 x =
	Empty
	
let put2 k v d =
	Entry(k, v, d)

let rec get2 k d =
	match d with
		Empty -> raise Not_found
	|	Entry(k', v', d') -> if k = k' then v'
							else (get2 k d')
	
(* Problem 2c
   empty3: unit -> ('a,'b) dict3
   put3: 'a -> 'b -> ('a,'b) dict3 -> ('a,'b) dict3
   get3: 'a -> ('a,'b) dict3 -> 'b
*)  

type ('a,'b) dict3 = ('a -> 'b)

let empty3 x = 
	(fun s -> raise Not_found)
	
let put3 k v d =
	(fun s -> if k = s then v
				else d s)
			
let get3 k d =
	d k
	
(* Calculators *)    
  
(* A type for arithmetic expressions *)
  
type op = Plus | Minus | Times | Divide
type aexp = Num of float | BinOp of aexp * op * aexp

(* Problem 3a
   evalAExp: aexp -> float
*)

(* This function calculates the correct float output, given the two operands, a and b, and the op type, o
*)
let calcExp a o b =
	match o with
		Plus -> a +. b
	|	Minus -> a -. b
	|	Times -> a *. b
	|	Divide -> a /. b

let rec evalAExp t = 
	match t with
		Num x -> x
	|	BinOp (x, y, z) -> match (x, y, z) with
							(Num(a), o, Num(b)) -> calcExp a o b
						|	(BinOp(a, b, c), o, Num(d)) -> calcExp (evalAExp(x)) o d
						|	(Num(a), o, BinOp(b, c, d)) -> calcExp a o (evalAExp(z))
						|	(BinOp(a, b, c), o, BinOp(d, e, f)) -> calcExp (evalAExp(x)) o (evalAExp(z))

(* A type for stack operations *)	  
	  
type sopn = Push of float | Swap | Calculate of op

(* Problem 3b
   evalRPN: sopn list -> float
*)

let evalRPN l =
	let rec evalRPNList (remain, sofar) =
		match (remain, sofar) with
			([], []) -> raise Not_found
		|	([], [x]) -> x
		|	(x::xs, []) -> (match x with
								Push z -> evalRPNList (xs, [z]))
		|	(x::xs, [y]) -> (match x with
								Push z -> evalRPNList (xs, z::[y])) 	
		|	(x::xs, y1::y2::rest) -> (match x with
										Push z -> evalRPNList (xs, z::sofar)
									|	Calculate z -> evalRPNList (xs, (calcExp y2 z y1)::rest)
									|	Swap -> evalRPNList (xs, y2::y1::rest))

	in evalRPNList (l, [])
  
(* Problem 3c
   toRPN: aexp -> sopn list
*)

let rec toRPN l =
	match l with
		Num x -> [Push x]
	|	BinOp(Num a, o, Num b) -> [Push a; Push b; Calculate o]
	|	BinOp(BinOp(a, b, c), o, Num(d)) -> toRPN(BinOp(a, b, c))@[Push d; Calculate o]
	|	BinOp(Num(a), o, BinOp(b, c, d)) -> [Push a]@toRPN(BinOp(b, c, d))@[Calculate o]
	|	BinOp(BinOp(a, b, c), o, BinOp(d, e, f)) -> toRPN(BinOp(a, b, c))@toRPN(BinOp(d, e, f))@[Calculate o]

  
(* Problem 3d
   toRPNopt: aexp -> (sopn list * int)
*)

(* This function counts the maximum number of consecutive pushes in the provided list by taking the list, l, 
	the current count, c, and the current max, m
*)
let rec maxStack (l, c, m) =
	match l with
		[] -> m
	|	x::xs -> match x with
					Push _ -> maxStack (xs, (c+1), m)
				|	_ -> if m > c then maxStack (xs, 0, m)
							else maxStack (xs, 0, c)
					
(* This function pushes the operands on the right hand side of the equation onto the stack first
	as opposed to the toRPN, which pushes the operands on the left onto the stack first
*)
let rec toRPNRight l =
	match l with
		Num x -> [Push x]
	|	BinOp(Num a, o, Num b) -> [Push a; Push b; Calculate o]
	|	BinOp(BinOp(a, b, c), o, Num(d)) -> if o = Minus || o = Divide then [Push d]@toRPNRight(BinOp(a, b, c))@[Swap]@[Calculate o]
												else [Push d]@toRPNRight(BinOp(a, b, c))@[Calculate o]
	|	BinOp(Num(a), o, BinOp(b, c, d)) -> if o = Minus || o = Divide then toRPNRight(BinOp(b, c, d))@[Push a]@[Swap]@[Calculate o]
												else toRPN(BinOp(b, c, d))@[Push a]@[Calculate o]
	|	BinOp(BinOp(a, b, c), o, BinOp(d, e, f)) -> if o = Minus || o = Divide then toRPN(BinOp(d, e, f))@toRPN(BinOp(a, b, c))@[Swap]@[Calculate o]
														else toRPN(BinOp(d, e, f))@toRPN(BinOp(a, b, c))@[Calculate o]
				

let toRPNopt l =
	if maxStack(toRPN l, 0, 0) < maxStack(toRPNRight l, 0, 0) then (toRPN l, maxStack(toRPN l, 0, 0))
		else (toRPNRight l, maxStack(toRPNRight l, 0, 0))
	
	
	
	