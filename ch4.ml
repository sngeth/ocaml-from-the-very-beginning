(* Write a function evens which does the opposite to odds, returning the even numbered
 * elements in a list. For example, evens[2; 4; 2; 4; 2] should return [4; 4]. What is the
 * type of your function?
 *)

(* [a] -> [a] *)
let rec evens l =
  match l with
   [] -> []
  | [a] -> []
  | _::a::t -> a :: evens t

(* Write a function count_true which counts the number of true elements in a list.
 * For example, count_true [true; false; true] should return 2. What is the type of your
 * function? Can you write a tail recursive function?
 *)

(* [bool] -> int *)
let rec _count_true l n =
  match l with
    [] -> n
  | true::t -> _count_true t (n + 1)
  | false::t -> _count_true t n

let count_true l = _count_true l 0
