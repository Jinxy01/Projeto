open Format
type kont =
  | Kid 
  | KConstructor_0 of int * kont 
let rec fat n =
  let rec fat_aux n kFunction =
    if n = 0
    then apply kFunction 1
    else fat_aux (n - 1) (KConstructor_0 (n, kFunction))
  
  and apply kFunction arg =
    match kFunction with
    | Kid  -> arg
    | KConstructor_0 (n,kFunction) ->
        let fatANF_0 = arg  in apply kFunction (n * fatANF_0)
   in fat_aux n Kid 
let n = 5 
let () = eprintf "fatorial_defunc: %d@." (fat n) 
let rec fat n =
  let rec fat_aux n kFunction =
    if n = 0
    then kFunction 1
    else fat_aux (n - 1) (fun fatANF_0  -> kFunction (n * fatANF_0))  in
  fat_aux n (fun s  -> s) 
let () = eprintf "fatorial_CPS   : %d@." (fat n) 
