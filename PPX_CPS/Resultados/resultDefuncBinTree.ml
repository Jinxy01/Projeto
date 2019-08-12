open Format
type t =
  | E 
  | N of t * t 
let rec leftist_tree t =
  function | 0 -> t | n -> leftist_tree (N (t, E)) (n - 1) 
let t = leftist_tree E 1_000_000 
type kont =
  | Kid 
  | KConstructor_1 of t * kont 
  | KConstructor_0 of int * kont 
let rec height t =
  let rec height_aux t kFunction =
    match t with
    | E  -> apply kFunction 0
    | N (t1,t2) -> height_aux t1 (KConstructor_1 (t2, kFunction))
  
  and apply kFunction arg =
    match kFunction with
    | Kid  -> arg
    | KConstructor_1 (t2,kFunction) ->
        let heightANF_0 = arg  in
        height_aux t2 (KConstructor_0 (heightANF_0, kFunction))
    | KConstructor_0 (heightANF_0,kFunction) ->
        let heightANF_1 = arg  in
        apply kFunction (1 + (max heightANF_0 heightANF_1))
   in height_aux t Kid 
let () = eprintf "height_defunc: %d@." (height t) 
let rec height t =
  let rec height_aux t kFunction =
    match t with
    | E  -> kFunction 0
    | N (t1,t2) ->
        height_aux t1
          (fun heightANF_0  ->
             height_aux t2
               (fun heightANF_1  ->
                  kFunction (1 + (max heightANF_0 heightANF_1))))
     in
  height_aux t (fun s  -> s) 
let () = eprintf "height_CPS   : %d@." (height t) 
