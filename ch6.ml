(* 1. Write a simple recursive function calm to replace exclamation marks in a
 * char list with periods. For example calm
 * ['H'; 'e'; 'l'; 'p'; '!'; ' '; 'F'; 'i'; 'r'; 'e'; '!'] should evaluate to
 * ['H'; 'e'; 'l'; 'p'; '.'; ' '; 'F'; 'i'; 'r'; 'e'; '.'].
 * Now rewrite your function to use map instead of recursion.
 * What are the types of your functions? *)

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

(* 3. Express your function cliplist again, this time using an anonymous function *)
let rec cliplist a_list =
  map (fun x ->
        if x < 1 then 1 else
          if x > 10 then 10 else x)
  a_list


(* 4. Write a function apply which, given another function, a number of times to
 * apply it, and an initial argument for the function, will return the cumulative
 * effect of repeatedly applying the function. For instance, apply f 6 4 should
 * return f (f (f (f (f (f 4)))))). What is the type of your function?
 *
 * apply : (a -> a) -> int a -> a -> a *)

let rec apply f n x =
  if n = 0
    then x
    else f (apply f (n - 1) x)

(* 5. Modify the insertion sort function from the preceding chpater to take a
 * comparison function, in the same way that we modified merge sort in this
 * chapter? What is its type?
 *
 *  insert : (a -> a -> bool) -> a -> a list -> a list
 *)

let rec insert f x l =
  match l with
    [] -> [x]
  | h::t ->
      if f x h
        then x :: h :: t
        else h :: insert f x t

 let rec sort f l =
   match l with
    [] -> []
  | h::t -> insert f h (sort f t)

(* Write a function filter which takes a function of type a -> bool and a list
 * and returns a list of just those elements of the argument list for which the
 * given function returns true *)

let rec filter f l =
  match l with
    [] -> []
  | h::t ->
      if f h
        then h::filter f t
        else filter f t
