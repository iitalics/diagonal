module Applicative(A: Intf.Applicative_S) =
  struct
    let ( <*> ) af ax = A.map2 (@@) af ax
    let ( >>| ) ax f = A.map f ax

    let rec all = function
      | []      -> A.const []
      | x :: xs -> A.map2 List.cons x (all xs)
  end
