type t =
  | SProp of string 
  | Not of t 
  | Ou of t * t 
  | E of t * t 
  | Imp of t * t 
let rec printExp (exp : t) =
  match exp with
  | SProp s -> print_string s
  | Not e -> (print_string " ~("; printExp e; print_string ") ")
  | Ou (e1,e2) ->
      (print_string " (";
       printExp e1;
       print_string " \\/ ";
       printExp e2;
       print_string ") ")
  | E (e1,e2) ->
      (print_string " (";
       printExp e1;
       print_string " /\\ ";
       printExp e2;
       print_string ") ")
  | Imp (e1,e2) ->
      (print_string " (";
       printExp e1;
       print_string " -> ";
       printExp e2;
       print_string ") ")
  
let impFree (exp : t) =
  let rec impFree_aux (exp : t) kFunction =
    match exp with
    | Not e ->
        Not (impFree_aux e (fun impFreeANF_0  -> kFunction impFreeANF_0))
    | Ou (e1,e2) ->
        Ou
          ((impFree_aux e1 (fun impFreeANF_0  -> kFunction impFreeANF_0)),
            (impFree_aux e2 (fun impFreeANF_0  -> kFunction impFreeANF_0)))
    | E (e1,e2) ->
        E
          ((impFree_aux e1 (fun impFreeANF_0  -> kFunction impFreeANF_0)),
            (impFree_aux e2 (fun impFreeANF_0  -> kFunction impFreeANF_0)))
    | Imp (e1,e2) ->
        Ou
          ((Not
              (impFree_aux e1 (fun impFreeANF_0  -> kFunction impFreeANF_0))),
            (impFree_aux e2 (fun impFreeANF_0  -> kFunction impFreeANF_0)))
    | _ -> kFunction exp  in
  impFree_aux exp (fun s  -> s) 
let exp =
  Imp
    ((Imp ((Ou ((SProp "p"), (Not (SProp "s")))), (SProp "q"))),
      (Imp ((SProp "p"), (E ((SProp "r"), (Not (SProp "s")))))))
  
let () = print_endline "Exp"; printExp exp; print_endline "" 
let () =
  print_endline "ImpFree Exp"; printExp (impFree exp); print_endline "" 
let nnfc (exp : t) =
  let rec nnfc_aux (exp : t) kFunction =
    match exp with
    | Not (Not e) -> nnfc_aux e (fun nnfcANF_0  -> kFunction nnfcANF_0)
    | Not (E (e1,e2)) ->
        Ou
          ((nnfc_aux (Not e1) (fun nnfcANF_0  -> kFunction nnfcANF_0)),
            (nnfc_aux (Not e2) (fun nnfcANF_0  -> kFunction nnfcANF_0)))
    | Not (Ou (e1,e2)) ->
        E
          ((nnfc_aux (Not e1) (fun nnfcANF_0  -> kFunction nnfcANF_0)),
            (nnfc_aux (Not e2) (fun nnfcANF_0  -> kFunction nnfcANF_0)))
    | Ou (e1,e2) ->
        Ou
          ((nnfc_aux e1 (fun nnfcANF_0  -> kFunction nnfcANF_0)),
            (nnfc_aux e2 (fun nnfcANF_0  -> kFunction nnfcANF_0)))
    | E (e1,e2) ->
        E
          ((nnfc_aux e1 (fun nnfcANF_0  -> kFunction nnfcANF_0)),
            (nnfc_aux e2 (fun nnfcANF_0  -> kFunction nnfcANF_0)))
    | _ -> kFunction exp  in
  nnfc_aux exp (fun s  -> s) 
let exp = Not (Ou ((Not (Ou ((SProp "p"), (Not (SProp "s"))))), (SProp "q"))) 
let () = print_endline "\nExp" 
let () = printExp exp; print_endline "" 
let () = print_endline "NNFC Exp" 
let () = printExp (nnfc exp); print_endline "" 
let distr ((e1 : t),(e2 : t)) =
  let rec distr_aux ((e1 : t),(e2 : t)) kFunction =
    match (e1, e2) with
    | (E (e11,e12),_) ->
        E
          ((distr_aux (e11, e2) (fun distrANF_0  -> kFunction distrANF_0)),
            (distr_aux (e12, e2) (fun distrANF_0  -> kFunction distrANF_0)))
    | (_,E (e21,e22)) ->
        E
          ((distr_aux (e1, e21) (fun distrANF_0  -> kFunction distrANF_0)),
            (distr_aux (e1, e22) (fun distrANF_0  -> kFunction distrANF_0)))
    | _ -> Ou ((kFunction e1), (kFunction e2))  in
  distr_aux (e1, e2) (fun s  -> s) 
let cnfc (exp : t) =
  let rec cnfc_aux (exp : t) kFunction =
    match exp with
    | Ou (e1,e2) ->
        cnfc_aux e2
          (fun cnfcANF_0  ->
             cnfc_aux e1 (fun cnfcANF_1  -> kFunction (distr (cnfcANF_1, cnfcANF_0))))
    | E (e1,e2) ->
        E
          ((cnfc_aux e1 (fun cnfcANF_0  -> kFunction cnfcANF_0)),
            (cnfc_aux e2 (fun cnfcANF_0  -> kFunction cnfcANF_0)))
    | _ -> kFunction exp  in
  cnfc_aux exp (fun s  -> s) 
let exp =
  E
    ((Ou ((SProp "p"), (E ((SProp "q"), (SProp "r"))))),
      (Ou ((E ((SProp "r"), (SProp "s"))), (SProp "t"))))
  
let () = print_endline "\nExp" 
let () = printExp exp; print_endline "" 
let () = print_endline "CNFC Exp" 
let () = printExp (cnfc exp); print_endline "" 
let algoritmoT (exp : t) = cnfc (nnfc (impFree exp)) 
let exp =
  Imp
    ((E ((Not (SProp "p")), (SProp "q"))),
      (E ((SProp "p"), (Imp ((SProp "r"), (SProp "q"))))))
  
let () = print_endline "\nExp" 
let () = printExp exp; print_endline "" 
let () = print_endline "Algoritmo T Exp" 
let () = printExp (algoritmoT exp); print_endline "" 
