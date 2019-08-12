open Format
let rec create_list acc =
  function | 0 -> List.rev acc | n -> create_list (n :: acc) (n - 1) 
let l = create_list [] 1_000_000 
type kont =
  | Kid 
  | KConstructor_0 of int * kont 
let rec sum list =
  let rec sum_aux list kFunction =
    match list with
    | [] -> apply kFunction 0
    | el::rx -> sum_aux rx (KConstructor_0 (el, kFunction))
  
  and apply kFunction arg =
    match kFunction with
    | Kid  -> arg
    | KConstructor_0 (el,kFunction) ->
        let sumANF_0 = arg  in apply kFunction (el + sumANF_0)
   in sum_aux list Kid 
let () = eprintf "sum_defunc: %d@." (sum l) 
let rec sum list =
  let rec sum_aux list kFunction =
    match list with
    | [] -> kFunction 0
    | el::rx -> sum_aux rx (fun sumANF_0  -> kFunction (el + sumANF_0))  in
  sum_aux list (fun s  -> s) 
let () = eprintf "sum_CPS   : %d@." (sum l) 
