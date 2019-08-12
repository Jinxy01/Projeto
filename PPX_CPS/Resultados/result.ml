type t =
  | E 
  | N of t * t 
let rec create_list acc =
  function | 0 -> List.rev acc | n -> create_list (n :: acc) (n - 1) 
let l = create_list [] 1_000_000 
type kont =
  | Kid 
  | KConstructor_0 of int * kont 
let rec sum matchArg =
  let rec sum_aux matchArg kFunction =
    match matchArg with
    | [] -> kFunction 0
    | el::rx -> sum_aux rx (fun sumANF_0  -> kFunction (el + sumANF_0))  in
  sum_aux matchArg (fun s  -> s) 
let time f x name =
  let t = Sys.time ()  in
  let _ = f x  in
  Format.eprintf "Execution time of %s: @. %fs @." name ((Sys.time ()) -. t) 
let () = time sum l "sum (l)" 
let mem_used = Gc.allocated_bytes () 
let () = Format.eprintf "Valor = %f MB@." ((mem_used /. 1024.0) /. 1024.0) 
