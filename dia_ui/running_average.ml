type t =
  { samples: float array;
    mutable sum: float;
    mutable avg: float option;
    mutable idx: int }

let make n_samples =
  { samples = Array.make n_samples 0.;
    sum = 0.;
    idx = 0;
    avg = None }

let n_samples t = Array.length t.samples
let average t = t.avg

let push x t =
  let n_samp = t |> n_samples in
  let sum' = t.sum -. t.samples.(t.idx) +. x in
  let idx' = t.idx + 1 in
  t.samples.(t.idx) <- x;
  t.sum <- sum';
  if idx' < n_samp then
    t.idx <- idx'
  else
    ( t.idx <- 0;
      t.avg <- Some (sum' /. float_of_int n_samp) )
