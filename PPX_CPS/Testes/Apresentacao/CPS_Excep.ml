
exception Negative

let %CPS rec fact x =
    if x < 0 then raise Negative
    else if x > 20 then failwith "Valor de x demasiado elevado!"
    else if x = 0 then 1 else x * fact (x -1)

let x = 5

let %CPS result = try fact x with 
    Negative -> Format.eprintf "Numero negativo!@."; exit 0

let () = Format.eprintf "Fatorial de %d = %d@." x result


(* Exemplo List *)

let %CPS rev_map2 f l1 l2 =
  let rec rmap2_f accu l1 l2 =
    match (l1, l2) with
    | ([], []) -> accu
    | (a1::l1, a2::l2) -> rmap2_f (f a1 a2 :: accu) l1 l2
    | (_, _) -> invalid_arg "List.rev_map2"
  in
  rmap2_f [] l1 l2

let l1 = [1;2]
let l2 = [2;3]
let l3 = [1;2;3]

let l = rev_map2 (+) l1 l3

let () = 
    Format.eprintf "Rev_map2 = [ ";
    List.iter (fun s -> Format.eprintf "%d; " s) l; 
    Format.eprintf "]@.";
