
(* CPS *)
let rec sum list k =
  match list with
  | [] -> k 0
  | el::rx -> sum rx (fun sum -> k (sum + el))
;;

(* Defuncionalization *)

type kontSum =
  | Kid
  | KN of int * kontSum

let rec sum_def list k =
  match list with
  | [] -> apply k 0
  | el::rx -> sum_def rx (KN (el,k))

and apply k args = match k with
  | Kid -> args
  | KN (el,k) -> let sum = args in apply k (sum+el)


(* -------------------------------------------------------------------- *)

(* CPS *)
let rec total list k =
  match list with
  | [] -> k 0
  | _::rx -> total rx (fun t -> k (1 + t))
;;

(* Defuncionalization *)

type kontTotal =
  | Kid
  | KN of int * kontTotal

let rec total_def list k =
  match list with
  | [] -> apply k 0
  | _::rx -> total_def rx (KN (1,k))

and apply k args = match k with
  | Kid -> args
  | KN (n1,k) -> let t = args in apply k (n1 + t) 


(* -------------------------------------------------------------------- *)

(* Criar lista *)

let rec create_list acc = function
  | 0 -> List.rev acc
  | n -> create_list (n::acc) (n - 1)

(* Variaveis a usar *)
let l1 = create_list [] 1_000_000

let () = 
  let open Format in
    eprintf "total_CPS: %d@." (total l1 (fun x -> x) );
    eprintf "total_Def: %d@." (total_def l1 Kid);
    eprintf "sum_CPS: %d@." (sum l1 (fun x -> x) );
    eprintf "sum_Def: %d@." (sum_def l1 Kid)
  ;;