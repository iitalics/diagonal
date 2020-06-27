(* applicative *)

module type Applicative_S = sig
  type 'a t
  val const: 'a -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val map2: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end

module Applicative(A: Applicative_S) =
  struct
    let ( <*> ) af ax = A.map2 (@@) af ax
    let ( >>| ) ax f = A.map f ax
    let both ax ay = A.map2 (fun x y -> x, y) ax ay

    let rec all = function
      | []      -> A.const []
      | x :: xs -> A.map2 List.cons x (all xs)
  end

(* list *)

module List = struct
  let[@ocaml.inline] sum_by f xs =
    List.fold_left (fun s x -> s + f x) 0 xs

  let rec merge_fold ~compare fx fy fxy acc xs ys =
    match xs, ys with
    | x :: xs', y :: ys' ->
       let c = compare x y in
       if c < 0 then
         merge_fold ~compare fx fy fxy (fx acc x) xs' ys
       else if c > 0 then
         merge_fold ~compare fx fy fxy (fy acc y) xs ys'
       else
         merge_fold ~compare fx fy fxy (fxy acc x y) xs' ys'
    | [], y :: ys' ->
       merge_fold ~compare fx fy fxy (fy acc y) [] ys'
    | x :: xs', [] ->
       merge_fold ~compare fx fy fxy (fx acc x) xs' []
    | [], [] ->
       acc

  let[@ocaml.inline] rev_merge compare xs ys =
    merge_fold ~compare
      (fun xs x   -> x :: xs)
      (fun xs   y -> y :: xs)
      (fun xs x y -> x :: y :: xs)
      [] xs ys

  let rev_split xys =
    let rec loop xs ys = function
      | (x, y) :: xys ->
         loop (x :: xs) (y :: ys) xys
      | [] ->
         (xs, ys)
    in
    loop [] [] xys

  let rec rev_map_append f xs ys =
    match xs with
    | []      -> ys
    | x :: xs -> rev_map_append f xs (f x :: ys)

  include List
end

(* option *)

module Option = struct
  module M0 = struct
    type 'a t = 'a option
    let const x = Some(x)
    let map = Option.map
    let map2 f ox oy = match ox with
      | None -> None
      | Some(x) -> match oy with
                   | None -> None
                   | Some(y) -> Some(f x y)
  end
  include M0
  include Option
  include Applicative(M0)
end
