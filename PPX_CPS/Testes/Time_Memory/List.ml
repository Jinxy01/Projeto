
(* Criar lista *)
let rec create_list acc = function
  | 0 -> List.rev acc
  | n -> create_list (n::acc) (n - 1)

let l = create_list [] 1_000_000

type kont = 
  | Kid
  | KConstructor_0 of int * kont

let %CPS rec sum list =
  match list with
  | [] -> 0
  | el::rx -> el + sum rx

(* Função timer *)
let time f x name =
    let t = Sys.time() in
    let _ = f x in
    Format.eprintf "Execution time of %s: @. %fs @." name (Sys.time() -. t);
;;

let () = time sum l "sum (l)"


(* Módulo Gc = https://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html
 Return the total number of bytes allocated since the program was started *)
let mem_used = Gc.allocated_bytes ()
let () = Format.eprintf "Valor = %f MB@." (mem_used/.1024.0/.1024.0)
