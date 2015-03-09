 type 'a my_list =
	| Item  of ('a * 'a my_list) 
	| Empty;;

let rec length = function
	| Item(a, liste) -> 1 + length liste
	| Empty -> 0

let hd = function
	| Empty -> failwith "hd"
	| Item(a, liste) -> a

let tl = function
	| Empty -> failwith "tl"
	| Item(a, liste) -> liste

let rec nth liste index = match liste with
	| Empty -> failwith "nth"
	| Item(a, liste) -> 
		match index with
		| index when index < 0 -> invalid_arg "get_nth"
		| 0 -> a
		| _ -> nth liste (index-1)   

let rev l =
	let rec helper l revl = match l with
	| Empty -> revl
	| Item(a, liste) -> helper liste (Item(a, revl))
	in helper l Empty;;

let rec append  src dest = match src, dest with
	| Empty, dest -> dest
	| Item(a, src), dest -> Item(a, (append src dest));;

let rev_append src dest = append dest (rev src) ;;

let flatten dbl_list =
		let rec doer dbl_list ret = match dbl_list with
		| Empty -> ret
		| Item(a, liste) -> doer liste (append ret a)
		in doer dbl_list Empty;;


let rec iter (func, liste) = match liste with
	| Empty -> ()
	| Item(a, liste) ->
	  begin
		func a;
	    	iter (func, liste) 
	  end
;;

let rec map func = function
	| Empty -> Empty
	| Item(a, liste) -> Item(func a, map func liste)


let rec fold_left func ret liste = match liste with
	| Empty -> ret
	| Item(a, liste) -> fold_left func (func ret a) liste

let rec for_all predicate = function
	| Empty -> true
	| Item(a, liste) -> if predicate a = false then false else for_all predicate liste

let rec exists predicate = function
	| Empty -> false
	| Item(a, liste) -> if predicate a = true then true else exists predicate liste

let rec mem value liste = match liste with
	| Empty -> false
	| Item(a, liste) -> if a = value then true else mem value liste 

let rec memq value liste = match liste with
	| Empty -> false
	| Item(a, liste) -> if a == value then true else mem value liste 

let filter predicate liste =
    let rec doer predicate liste ret = match liste with
	| Empty -> ret
	| Item(a, liste) -> if predicate a = true then Item(a, ret) else doer predicate liste ret
	in doer predicate liste Empty				   

let getval1 (value1, value2) = value1

let getval2 (value1, value2) = value2

let rec mem_assoc value = function
	| Empty -> false
	| Item(a, liste) -> if (getval1 a) = value then true else mem_assoc value liste

let rec assoc value = function
	| Empty -> failwith "Not_found"
	| Item(a, liste) -> if (getval1 a) = value then getval2 a else assoc value liste

let split liste =
	let rec splitter liste ret1 ret2 = match liste with
	| Empty -> (rev ret1, rev ret2)
	| Item(a, liste) -> splitter liste (Item((getval1 a), ret1)) (Item((getval2 a), ret2))
	in splitter liste Empty Empty

let remove_assoc value liste = 
  let rec remover value liste ret = match liste with
	| Empty -> ret
	| Item(a, liste) -> 
	   if (getval1 a) = value 
	   then remover value liste ret 
	   else Item(a, remover value liste ret)
	in remover value liste Empty
