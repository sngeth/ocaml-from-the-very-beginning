(* 1. Write a function evens which does the opposite to odds, returning the even
 * numbered elements in a list. For example, evens[2; 4; 2; 4; 2] should return
 * [4; 4]. What is the type of your function?
 *)

(* [a] -> [a] *)
let rec evens l =
  match l with
   [] -> []
  | [_] -> []
  | _::a::t -> a :: evens t

(* 2. Write a function count_true which counts the number of true elements in a
 * list. For example, count_true [true; false; true] should return 2. What is the
 * type of your function? Can you write a tail recursive function?
 *)

(* [bool] -> int *)
let rec _count_true l n =
  match l with
    [] -> n
  | true::t -> _count_true t (n + 1)
  | false::t -> _count_true t n

let count_true l = _count_true l 0

(* 3. Write a function which, given a list, builds a palindrome from it.
 * A palindrome is a list which equals its own reverse. You can assume the
 * existence of rev and @. Write another fuction which determins if a list
 * is a palindrome *)

let make_palindrome l =
  l @ rev l

let is_palindrome l =
  l = rev l

(* 4. Write a function drop_last which returns all but the last element
 * of a list. If the list is empty, it should return the empty list.
 * So for example, drop_last [1; 2; 4; 8] should return [1; 2; 4].
 * What about a tail recursive version?
 *)

let rec drop_last l =
  match l with
    []   -> []
  | [_]  -> []
  | h::t -> h :: drop_last l

(* tail recursive function of drop_last *)
let rec _drop_last a l =
  match l with
    [] -> rev a
  | [] -> rev a
  | h::t -> _drop_last (h::a) t

let drop_last l =
  _drop_last [] l

(* 5. Write a function member of type a -> a list -> bool which returns
 * true if an element exists in a list, or false if not. For example,
 * member 2 [1; 2; 3] should evaluate to true, but member 3 [1; 2]
 * should evaluate to false. *)

let member e l =
  match l with
    [] -> false
  | h::t -> h = e || member e t

(* 6. Use your member function to write a function make_Set which, given a list,
 * returns a list which contains all the elements of the original list, but has
 * no duplicate elements. For example, make_set[1; 2; 3; 3; 1] might return
 * [2; 3; 1]. What is the type of your function? *)

(* [int] -> [int] *)
let rec _make_set l new_list =
  match l with
    [] -> []
  | h::t -> if member h t then make_set t else h :: make_set t
