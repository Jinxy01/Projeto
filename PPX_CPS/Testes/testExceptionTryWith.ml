(* Local de testes genéricos*)
exception Negative3
exception Negative2
exception Ten

let %CPS rec fact x =
    if x < -10 then raise Negative2
    else if x < -5 then failwith "Menos de -5 e mais de -10"
    else if x < 0 then raise Negative3
    else if x = 0 then 1 else x * fact (x -1)

let x = -22
let %CPS n = 
    try fact x
    with 
    | Negative3 -> Format.eprintf "Erro@."; exit 0
    | Negative2 -> -2
    | Not_found -> 2

let () = Format.eprintf "Valor: %d@." n

let x = 5
let %CPS rec dez x =
    if x > 10 then raise Negative2
    else if x = 10 then raise Ten
    else if x < 0 then assert false 
    else if x = 0 then 0 else dez (x-1)

let %CPS n = 
    try dez x
    with Ten -> Format.eprintf "10.0 @."; exit 0
    | Negative2 -> (-5)

(* let n = fact x *)
let () = Format.eprintf "Valor: %d@." n

let %CPS rec fact n =
  if n <= 1 then 1
  else n *  fact (n - 1)


(* Erro com kException ID *)
let %CPS rev_map2 f l1 l2 =
  let rec rmap2_f accu l1 l2 =
    match (l1, l2) with
    | ([], []) -> accu
    | (a1::l1, a2::l2) -> rmap2_f (f a1 a2 :: accu) l1 l2
    | (_, _) -> invalid_arg "List.rev_map2"
  in
  rmap2_f [] l1 l2



