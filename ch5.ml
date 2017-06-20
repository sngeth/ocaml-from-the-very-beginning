(* Chapter 5 Exercises *)
(* 1. In msort, we calculate the value of the expression length 1 / 2 twice.
 * Modify msort to remove this inefficency *)

let rec merge x y =
  match x, y with
    [], l -> l
  | l,  [] -> l
  | hx::tx, hy::ty ->
      if hx < hy
      then hx :: merge tx (hy :: ht)
      else hy :: merge (hx :: tx) ty

let rec msort l =
  match l with
    []  -> []
  | [x] -> [x]
  | _   ->
      let x = length l / 2 in
        let left = take x l in
          let right = drop x l in
            merge (msort left) (msort right)

(* 3. Write a version of insertion sort which sorts the argument list into reverse order
 * We just need to flip the comparison operator. *)

let rec insert x l =
  match l with
    [] -> [x]
  | h::t ->
      if x >= h
        then x :: h :: t
        else h :: insert x t

let rec sort l =
  match l with
    [] -> []
  | h::t -> insert h (sort t)
