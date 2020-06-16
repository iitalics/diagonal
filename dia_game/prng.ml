module List = Util.List

module Xorshift(P: sig
                    val a: int
                    val b: int
                    val c: int
                  end)
  =
  struct
    open P

    (* xorshift* from Xorshift RNGs:
     * https://www.jstatsoft.org/article/view/v008i14
     *)

    type t = int64 ref
    type +'a gen = t -> 'a

    let make ~(seed: int): t =
      ref @@ Int64.(of_int seed
                    |> logor 1L
                    |> mul 0x2545F4914F6CDD1DL)

    let copy g =
      ref !g

    let rand_int64 : int64 gen =
      fun g ->
      let x = !g in
      let x = Int64.(shift_left  x a |> logxor x) in
      let x = Int64.(shift_right x b |> logxor x) in
      let x = Int64.(shift_left  x c |> logxor x) in
      g := x; x
  end

include Xorshift
          (struct
            let a, b, c = 13, 7, 17
          end)

let[@ocaml.inline] rand_int30 g =
  Int64.(logand (rand_int64 g) 0x7FFFFFFFL |> to_int)

(* let rand_int30 g =
 *   let z = rand_int30 g in
 *   print_endline (Printf.sprintf "PRNG gave (30 bits): %x" z); z *)

let[@ocaml.inline] rand_int ~max : int gen =
  fun g -> (rand_int30 g) mod max

(* let rand_int ~max g =
 *   let z = rand_int ~max g in
 *   print_endline (Printf.sprintf "PRNG gave (bounded): %d" z); z *)

let[@ocaml.inline] rand_pick (arr: 'a array) : 'a gen =
  fun g -> Array.get arr (rand_int ~max:(Array.length arr) g)

let rand_pick_weighted ws =
  fun g ->
  let rec find i = function
    | [] -> failwith "UNREACHABLE: out of bounds!"
    | (x, n) :: xs ->
       if i < n then x else find (i - n) xs
  in
  let max = ws |> List.sum_by snd in
  find (rand_int ~max g) ws
