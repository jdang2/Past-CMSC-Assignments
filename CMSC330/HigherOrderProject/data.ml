open Funs

(***********************)
(* Part 2: Integer BST *)
(***********************)

type int_tree =
  | IntLeaf
  | IntNode of int * int_tree * int_tree

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(x, IntLeaf, IntLeaf)
  | IntNode (y, l, r) when x > y -> IntNode (y, l, int_insert x r)
  | IntNode (y, l, r) when x = y -> t
  | IntNode (y, l, r) -> IntNode (y, int_insert x l, r)

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (y, l, r) when x > y -> int_mem x r
  | IntNode (y, l, r) when x = y -> true
  | IntNode (y, l, r) -> int_mem x l

(* Implement the functions below. *)

let rec int_size t = 
  match t with
  IntLeaf -> 0
  | IntNode (y, l, r) -> 1 + (int_size l) + (int_size r)
;;

let rec int_max t = 
  match t with
  IntLeaf -> invalid_arg "int_max"
  | IntNode (y, l, IntLeaf) -> y
  | IntNode (y, l, r) -> int_max r
;;

let rec int_common t x y = 
  if (int_mem x t) == false || (int_mem y t) == false then invalid_arg "int_common" else
    match t with 
    IntLeaf -> invalid_arg "int_common"
    | IntNode (i, l, r) -> 
      if (x <= i && y >= i) || (x >= i && y <= i) then i
      else if x < i && y < i then int_common l x y
      else int_common r x y
;;

(***************************)
(* Part 3: Polymorphic BST *)
(***************************)

type 'a atree =
    Leaf
  | Node of 'a * 'a atree * 'a atree
;;
type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

(* Implement the functions below. *)

let rec pinsertaux x comp t = 
  match t with 
  Leaf -> Node (x, Leaf, Leaf)
  | Node (y, l, r) when (comp x y) = 0 -> t
  | Node (y, l, r) when (comp x y) < 0 -> Node (y, pinsertaux x comp l, r)
  | Node (y, l, r) -> Node (y, l, pinsertaux x comp r)
;;

let pinsert x t = 
  match t with
  | compfn, atree -> (compfn, pinsertaux x compfn atree)
;;
   
let rec pmemaux x comp t = 
  match t with 
  Leaf -> false
  | Node (y, l, r) when (comp x y) = 0 -> true 
  | Node (y, l, r) when (comp x y) < 0 -> pmemaux x comp l 
  | Node (y, l, r) -> pmemaux x comp r 
;;
let pmem x t = 
  match t with 
  | compfn, atree -> pmemaux x compfn atree
;;

let pinsert_all lst t = 
  fold(fun tree x -> pinsert x tree) t lst
;;


let rec p_as_list t = 
  let rec traverse t acc =
    match t with
    | (compfn, Leaf) -> acc
    | (compfn, Node(y, l, r)) -> let right = traverse (compfn, r) acc in
                                 let visit = y::right in
                                 traverse (compfn, l) visit in
                                 traverse t []
  ;;

let pmap f t = 
  match t with 
  | (compfn, atree) -> pinsert_all (map f (p_as_list t)) (compfn, Leaf)
;;
  
 
(***************************)
(* Part 4: Variable Lookup *)
(***************************)

type lookup_table = (int * string) list list
;;

let empty_table () : lookup_table = []
;;
 
let push_scope (table: lookup_table) : lookup_table =
  [] :: table
;;

let rec pop_scope (table: lookup_table) : lookup_table = 
  match table with 
  [] -> failwith "No scopes remain!"
  | h :: t -> t 
;;


let rec add_var name value (table: lookup_table) : lookup_table =
  match table with
  [] -> failwith "There are no scopes to add a variable to!"
  | h :: t -> ((value, name) :: h) :: t
;;

let rec lookup name table =  
  match table with 
  [] -> failwith "Variable not found!"
  | h :: t -> let rec aux lst = 
              match lst with 
              [] -> failwith "fail"
              | (v, s) :: tl when name = s -> v 
              | (v, s) :: tl -> aux tl in
              try aux h with
              Failure _ -> lookup name t
;;
