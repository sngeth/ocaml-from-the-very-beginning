(* 1. Write a simple recursive function calm to replace exclamation marks in a char list with periods.
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

(* 2. Write a function clip which, given an integer, clips it to the range
 * 1...10 so that integers bigger than 10 round down to 10, and those smaller
 * than 1 round up to 1. Write another function cliplist which uses this first
 * function together with map to apply this clipping to a whole list of
 * integers. *)

let clip x =
  if x < 1 then 1 else
    if x > 10 then 10 else x

let rec cliplist a_list =
  map clip a_list
