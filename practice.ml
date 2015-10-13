(* Returns e if list l has e *)
let rec has_element l e = match l with
	| [] -> false
	| h::t -> if h = e true else has_element t e 

(* DUplicates all elements of a list into a new list*)
let rec duplicate_elements l = match l with
	| [] -> []
	| h::t -> h :: h :: duplicate_elements t

(* Reverse a list *)
let rec reverse l = match l with
	| [] -> []
	| h::t -> reverse t @ [h]

(* TAIL RECURSIVE FORM *)
let rec reverse_tr l acc = match l with
	| [] -> acc
	| h::t -> reverse t (acc @ [h])

(* Find element at index n in list l*)
let rec find_element_n l n = match l with
	| [] -> raise (failure "Empty list")
	| h::t -> if n = 0 then h else find_element_n t (n - 1)

(* TAIL RECURSIVE FORM *)
let rec num_elements_tr l acc = match l with
	| [] -> acc
	| h::t -> num_elements t (acc + 1)

(* Range of numbers between a an b *)
let rec range a b result = 
	if a = b then result
	else range (a + 1) b (a :: result)

(* Storing functions in lists *)
let my_list = [my_fun1 ; my_fun2 ; my_fun3]

(* Apply f to list *)
let rec apply f my_list result = match my_list with
	| [] -> result
	| h::t -> apply f t (f h :: result)

(* Head of list *)
let head l = match l with 
	| [] -> raise(failure "Empty list")
	| h::t -> h

(* Tail of list *)
let tail 1 = match l with
	| [] -> []
	| h::t -> t

(* Concatenate two lists *)
let rec append l1 l2 = match l1, l2 with 
	| [], [] -> []
	| [], l -> l
	| l, [] -> l
	| h::t, l -> h::append t l 

(* reverse l1 and concatenate to l2 *)
let rec rev_append l1 l2 = match l1, l2 with
	| [], [] -> []
	| [], l -> l
	| l, [] -> l
	| h::t, l -> rev_append t h::l

(* TAIL RECURSIVE FORM *)
let rec rev_append_tr l1 l2 acc = match l1, l2 with
	| [], [] -> []
	| [], l -> 
	| l, [] -> 
	| h::t, l -> 
	
(* Concat list of lists *)
let rec concat l = match l with 
	| [] -> []
	| h::t -> append h (concat t)

(* Iterators *)
let rec map f l = match l with
	| [] -> []
	| h::t -> f h :: map f t 

let my_func l = match l with
	| [] -> []
	| h::t -> h + 2 :: my_func t 
	

(* Map at index i; Not finished *)
let rec mapi f l = match l with 
	| [] -> []
	| h::t -> f h + 2 :: mapi f t 

(* Map with reversed list *)
let rec rev_map f l = match l with
	| [] -> []
	| h::t -> my_func (reverse_tr l [] )

(* Fold left on list *)
let rec fold_left f acc l = match l with
	| [] -> acc
	| h::t -> fold_left f (f acc h) t

(* Fold right on list*)
let rec fold_right f acc l = match l with 
	| [] -> acc
	| h::t -> f h (fold_right f acc t)

(* Combine two lists into list containing tuples *)
let rec combine a b = match (a, b) with 
	| (h1::t1, h2::t2) -> (h1, h2) :: combine t1 t2
	| (_, _) -> []

(* Split list of tuples into pair of lists*)
let rec split a = match a with
	| [] -> ([], [])
	| (h1, h2)::t -> let (xs, ys) = split t in (h1::xs, h2::ys)





