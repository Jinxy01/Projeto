open Format 

type t = E | N of t * t

(* Criar arvore *)
let rec leftist_tree t = function
  | 0 -> t
  | n -> leftist_tree (N (t, E)) (n - 1)
let t = leftist_tree E 1_000_000


let %CPS rec height t = match t with
  | E -> 0
  | N (t1, t2) -> 1 + max (height t1) (height t2)

let () = eprintf "height_CPS   : %d@." (height t)