(* Write a simple recursive function calm to replace exclamation marks in a char list with periods.
 * For example calm ['H'; 'e'; 'l'; 'p'; '!'; ' '; 'F'; 'i'; 'r'; 'e'; '!'] should evaluate to
 * calm ['H'; 'e'; 'l'; 'p'; '.'; ' '; 'F'; 'i'; 'r'; 'e'; '.'].
 * Now rewrite your function to use map instead of recursion. What are the types of your functions? *)

let rec calm l =
  match l with
    [] -> []
  | '!'::t -> '.' :: calm t
  | h::t -> h :: calm t


let calm_char x =
  match x with '!'-> '.' | _ -> x

let calm l =
  map calm_char l
