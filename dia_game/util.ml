module List = struct
  let[@ocaml.inline] sum_by f xs =
    List.fold_left (fun s x -> s + f x) 0 xs

  include List
end
