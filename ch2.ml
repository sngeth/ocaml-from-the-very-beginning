(* 1. Write a function which multiplies a given number by then. What is its type? *)
let mult_by_10 num = num * 10
(* val mult_by_10 : int -> int *)

(* 2. Write a function which returns true if both arguments are non-zero, and falsoe otherwise. What is its type? *)
let both_non_zero x y = if x > 0 && y > 0 then true else false
(* val both_non_zero : int -> int -> bool *)

(* 3. Write a recursive function which, given a number n,
   calculates the sum 1 + 2 + 3 + .. + n. What is its type? *)
let rec sum num =
  if num = 1 then 1 else
    num + sum(num - 1)
(* val sum : int -> int *)

(* 4. Write a function power x n which raises x to the power n. Give its type. *)
let pow x n = x ** n
(* val pow: float -> float -> float *)

(* 5. Write a function isconsonant which, given a lower-case char is in the range 'a'..'z',
 * determines if it is a conosonant. *)
let isconsonant c =
  match c with
    'a' | 'e' | 'i' | 'o' | 'u' -> false
  | 'a'..'z' -> true
(* val isconsonant : char -> bool *)
