type t = E | N of t * t

(* Criar arvore *)
let rec leftist_tree t = function
  | 0 -> t
  | n -> leftist_tree (N (t, E)) (n - 1)

let t = leftist_tree E 1_000_000

type kont =
  | Kid
  | KConstructor_1 of t * kont
  | KConstructor_0 of int * kont

let %Defunc rec height t = match t with
  | E -> 0
  | N (t1, t2) -> 1 + max (height t1) (height t2)

(* Função timer *)
let time f x name =
    let t = Sys.time() in
    let _ = f x in
    Format.eprintf "Execution time of %s: @. %fs @." name (Sys.time() -. t)

let () = time height t "heightDefunc (t)"

let mem_used = Gc.allocated_bytes ()
let () = 
    Format.eprintf "Memoria = %f MB@." (mem_used/.1024.0/.1024.0)