type t = E | N of t * t

(* Criar arvore *)
let rec leftist_tree t = function
  | 0 -> t
  | n -> leftist_tree (N (t, E)) (n - 1)

(* Criar lista *)
let rec create_list acc = function
  | 0 -> List.rev acc
  | n -> create_list (n::acc) (n - 1)

(* Variaveis a usar *)
let t1 = leftist_tree E 1_000_00
let t2 = leftist_tree E 1_000_000

let l1 = create_list [] 1_000_00
let l2 = create_list [] 1_000_000


(* Função timer *)
let time f x name =
    let t = Sys.time() in
    let _ = f x in
    Format.eprintf "Execution time of %s: @. %fs @." name (Sys.time() -. t);
;;

(* Funções CPS *)
let %CPS rec height = function
  | E ->  0
  | N(a,b) -> 1 + max (height a) (height b) 
;;

let %CPS rec sum list =
  match list with
  | [] -> 0
  | el::rx -> el + sum rx

let %CPS rec fatorial = function
  | 1 -> 1
  | n -> n * fatorial (n-1)
;;

(* Testes *)

let () = time height t2 "heightCPS (t2)"
let () = time height t1 "heightCPS (t1)"

(* Retirar CPS de height *)
let rec height = function
  | E ->  0
  | N(a,b) -> 1 + max (height a) (height b) 
;;
let () = time height t1 "height (t1)"

let n = 8
let () = time fatorial n "fatorialCPS (8)"

(* Retirar CPS de fatorial *)
let rec fatorial = function
  | 1 -> 1
  | n -> n * fatorial (n-1)
;;
let () = time fatorial n "fatorial (8)"

let () = time sum l1 "sumCPS (l1)"

(* Retirar CPS de height *)
let rec sum list =
  match list with
  | [] -> 0
  | el::rx -> el + sum rx

let () = time sum l1 "sum (l1)"
;;

