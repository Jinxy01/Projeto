
(* Variáveis *)
let rec create_list acc = function
  | 0 -> List.rev acc
  | n -> create_list (n::acc) (n - 1)
let l = [1;2;3;2]

let l_100mil = create_list [] 1_000_00
let l_1milhao = create_list [] 1_000_000

(* Teste com folds *)

let rec fold_left f accu l =
  match l with
    [] -> accu
  | a::l -> fold_left f (f accu a) l

let %CPS rec fold_right f l accu =
  match l with
    [] -> accu
  | a::l -> f a (fold_right f l accu)

let lReturnFLeft = fold_left (fun a b -> a + b) 0 l_1milhao
let lReturnFRight = fold_right (fun a b -> a + b) l_1milhao 0 
let () = 
  let open Format in
    eprintf "Fold_left : %d@." lReturnFLeft;
    eprintf "Fold_right: %d@." lReturnFRight