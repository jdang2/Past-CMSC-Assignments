open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let count_occ lst target = 
  fold(fun acc x -> if x = target then acc + 1 else acc) 0 lst
;;

let uniq lst = 
  fold(fun acc x -> if count_occ acc x < 1 then x :: acc else acc) [] lst
;;

let assoc_list lst = 
  fold(fun acc x -> 
    if count_occ acc (x, count_occ lst x) < 1
    then (x, count_occ lst x):: acc 
    else acc) [] lst
;;