
(* CPS *)

let rec fib n k =
  if n <= 2 then k 1
  else fib (n-2) (fun n2 ->
       fib (n-1) (fun n1 -> k (n1 + n2))) 

(* Defuncionalization *)

type kont =
  | Kid
  | KN2 of int * kont
  | KN1 of int * kont

let rec fib_def n k =
  if n <= 2 then apply k 1
  else fib_def (n-2) (KN2 ((n-1),k))

and apply k arg = match k with
  | Kid -> arg
  | KN2 (a,k)-> let n1 = arg in fib_def a (KN1 (n1,k))
  | KN1 (n1,k) -> let n2 = arg in apply k (n1 + n2)

(* Variaveis a usar *)
let n = 10

let () = 
  let open Format in
    eprintf "fib_CPS: %d@." (fib n (fun x -> x) );
    eprintf "fib_Def: %d@." (fib_def n Kid)
  ;;
