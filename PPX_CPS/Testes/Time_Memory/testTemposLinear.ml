open Format

(* Funções auxiliares de criação, escrita e tempo *)

let time f x name =
    let t = Sys.time() in
    let _ = f x in
    Format.eprintf "Execution time of %s: @. %fs @." name (Sys.time() -. t);
    1_000_000
;;

let rec create_list acc = function
  | 0 -> List.rev acc
  | n -> create_list (n::acc) (n - 1)

let printList list =
  let () = eprintf "[" in
  let rec print = function
    | [] -> eprintf "]"
    | el::[] -> eprintf "%d" el; print []
    | el::rx -> eprintf "%d; " el; print rx
  in print list
;;
let l2 = create_list [] 1_000_000
let l3 = [1;2;2;3;4;1;2;2;1;3;4]


(* Testes *)

(* retirar distintos *)
let retiraIguais lista =
  let hashTable = Hashtbl.create 12345678 in
  let rec retiraAux lista acc =
    match lista with 
    | [] -> List.rev acc
    | el::rx -> 
        try 
        let _ = Hashtbl.find hashTable el in 
        retiraAux rx acc
        with Not_found -> Hashtbl.add hashTable el el; retiraAux rx (el::acc) 
  in retiraAux lista [] 
;;
 

let () = 
  eprintf "Lista antiga =  " ; printList l3;
  eprintf "\nLista nova   =  " ; printList (retiraIguais l3);
  eprintf "\n"
;;

(* Avaliar distintos *)

let avaliaDistintos lista =
  let hashTable = Hashtbl.create 12345678 in
  let rec avaliaAux lista =
    match lista with 
    | [] -> Hashtbl.length hashTable
    | el::rx -> 
        try 
        let _ = Hashtbl.find hashTable el in 
        avaliaAux rx
        with Not_found -> Hashtbl.add hashTable el el; avaliaAux rx
  in avaliaAux lista 
;;

let () = 
  let open Format in
    eprintf "Total (%d): %d@." (List.length l2) (time avaliaDistintos l2 "avaliaDistintos");
;;

(* Transformação para CPS *)


let %CPS avaliaDistintos lista =
  let hashTable = Hashtbl.create 12345678 in
  let rec avaliaAux lista =
    match lista with 
    | [] -> Hashtbl.length hashTable
    | el::rx -> 
        try 
        let _ = Hashtbl.find hashTable el in 
        avaliaAux rx
        with Not_found -> Hashtbl.add hashTable el el; avaliaAux rx
  in avaliaAux lista 
;;

let () = 
  let open Format in
    eprintf "Total (%d): %d@." (List.length l2) (time avaliaDistintos l2 "avaliaDistintosCPS");
;;


let () = 
  let open Format in
    eprintf "Total (%d): %d@." (List.length l2) (time retiraIguais l2 "retiraIguais")
;;

let %CPS retiraIguais lista =
  let hashTable = Hashtbl.create 12345678 in
  let rec retiraAux lista acc =
    match lista with 
    | [] -> List.rev acc
    | el::rx -> 
        try 
        let _ = Hashtbl.find hashTable el in 
        retiraAux rx acc
        with Not_found -> Hashtbl.add hashTable el el; retiraAux rx (el::acc) 
  in retiraAux lista [] 
;;

let () = 
  let open Format in
    eprintf "Total (%d): %d@." (List.length l2) (time retiraIguais l2 "retiraIguaisCPS"); 
