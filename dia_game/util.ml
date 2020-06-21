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

  include List
end
