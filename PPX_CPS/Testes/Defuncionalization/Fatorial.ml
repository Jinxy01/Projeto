
(* CPS *)

let rec fact n k =
  if n <= 1 then k 1
  else fact (n-1) (fun nK -> k (n * nK))

(* Defuncionalization *)

type kont =
  | Kid
  | KN of int * kont

let rec fact_def n k =
  if n <= 1 then apply k 1
  else fact_def (n-1) (KN (n,k)) 

and apply k arg = match k with
  | Kid -> arg
  | KN(nK,k) -> let n = arg in apply k (n * nK) 

(* Variaveis a usar *)
let n = 5

let () = 
  let open Format in
    eprintf "fib_CPS: %d@." (fact n (fun x -> x) );
    eprintf "fib_Def: %d@." (fact_def n Kid)
  ;;

