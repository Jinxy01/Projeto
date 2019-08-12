(* Variáveis *)
let rec create_list acc = function
  | 0 -> List.rev acc
  | n -> create_list (n::acc) (n - 1)

let l = [1;2;3;2]

let l_100mil = create_list [] 1_000_00
let l_1milhao = create_list [] 1_000_000


(* Teste simples com exceção *)

let %CPS rec map2 f l1 l2 =
  match (l1, l2) with
    ([], []) -> []
  | (a1::l1, a2::l2) -> let r = f a1 a2 in r :: map2 f l1 l2
  | (_, _) -> invalid_arg "List.map2"

let lReturn = map2 (fun x y -> x + y) l l

let () = 
  let open Format in
    eprintf "Map2: ";
    List.iter (fun s -> eprintf "%d, " s) lReturn;
    eprintf "@."
;;