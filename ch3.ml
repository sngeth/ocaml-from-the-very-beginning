(* Rewrite the not function from the previous chapter in pattern matching style *)
let not x =
  match x with
    true -> false
  | false -> true

(* Use pattern matching to write a recursive function which, given a positive integer n
 * returns the sum of all the integers from 1 to n.*)
let rec sum n =
  match n with
    1 -> 1
  | _ -> n + sum(n-1)

(* Use pattern matching to write a function which, given two numbers x and n, computes x^n *)
let pow x n =
  match n with
    0 -> 1
  | 1 -> x
  | _ -> x * pow x (n-1)


(* There is a special patern x..y to denote continuous ranges of characters,
 * for example 'a'..'z' will match all lower case letters. Write functions islower
 * and isupper, each of type char -> bool, to decide on the case of a given letter. *)
let islower c =
  match c with
    'a'..'z' -> true
    | _      -> false

let isupper c =
  match c with
    'A'..'Z' -> true
    | _      -> false

