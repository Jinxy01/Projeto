
(* Referência: aula 9, Lógica Computacional
 http://www.di.ubi.pt/~desousa/LC/at9.pdf *)

type t = 
  | SProp of string
  | Not of t
  | Ou of t * t
  | E of t * t
  | Imp of t * t

let rec printExp (exp : t) =
  match exp with
  | SProp(s) -> print_string s
  | Not(e) -> print_string (" ~("); (printExp e); print_string (") ")
  | Ou(e1,e2) -> print_string (" ("); (printExp e1); print_string (" \/ "); (printExp e2); print_string (") ")
  | E(e1,e2) -> print_string (" ("); (printExp e1); print_string (" /\\ "); (printExp e2); print_string (") ")
  | Imp(e1,e2) -> print_string (" ("); (printExp e1); print_string (" -> "); (printExp e2); print_string (") ")

(* ImpFree *)
let %CPS rec impFree (exp : t) =
  match exp with
  | Not(e) -> Not(impFree e)
  | Ou(e1,e2) -> Ou(impFree e1,impFree e2)
  | E(e1,e2) -> E(impFree e1,impFree e2)
  | Imp(e1,e2) -> Ou(Not(impFree e1),impFree e2)
  | _ -> exp

let exp = Imp(
    (Imp(
      Ou(SProp("p"),Not(SProp("s"))),
      SProp("q"))),
    (Imp
      (SProp("p"),
        (E(SProp("r"),(Not(SProp("s"))))))))

let () = print_endline "Exp"; (printExp exp); print_endline ""
let () = print_endline "ImpFree Exp"; (printExp (impFree exp)); print_endline ""

(* NNFC *)
let %CPS rec nnfc (exp : t) =
  match exp with
  | Not(Not(e)) -> nnfc(e)
  | Not(E(e1,e2)) -> Ou(nnfc(Not(e1)),nnfc(Not(e2)))
  | Not(Ou(e1,e2)) -> E(nnfc(Not(e1)),nnfc(Not(e2)))
  | Ou(e1,e2) -> Ou(nnfc e1,nnfc e2)
  | E(e1,e2) -> E(nnfc e1,nnfc e2)
  | _ -> exp

let exp = Not(
      Ou(
        Not(
          Ou(SProp("p"),Not(SProp("s")))
        ),
        SProp("q")
      )
    )

let () = print_endline "\nExp"
let () = (printExp exp); print_endline ""
let () = print_endline "NNFC Exp"
let () = (printExp (nnfc exp)); print_endline ""

(* CNFC e Distr *)
let %CPS rec distr ((e1 : t),(e2 : t)) =
  match (e1,e2) with
  | (E(e11,e12),_) -> E(distr(e11,e2),distr(e12,e2))
  | (_,E(e21,e22)) -> E(distr(e1,e21),distr(e1,e22))
  | _ -> Ou(e1,e2)

let %CPS rec cnfc (exp : t) =
  match exp with
  | Ou(e1,e2) -> distr(cnfc e1, cnfc e2)
  | E(e1,e2) -> E(cnfc e1, cnfc e2)
  | _ -> exp

let exp = E(
      Ou(
        SProp("p"),
        E(SProp("q"),SProp("r"))
      ),
      Ou(
        E(SProp("r"),SProp("s")),
        SProp("t")
      )
    )

let () = print_endline "\nExp"
let () = (printExp exp); print_endline ""
let () = print_endline "CNFC Exp"
let () = (printExp (cnfc exp)); print_endline ""

(* Algoritmo T *)
let algoritmoT (exp : t) = cnfc (nnfc (impFree exp))

let exp = Imp(
      E(
        Not(SProp("p")),
        SProp("q")
      ),
      E(
        SProp("p"),
        Imp(SProp("r"),SProp("q"))
      )
    )

let () = print_endline "\nExp"
let () = (printExp exp); print_endline ""
let () = print_endline "Algoritmo T Exp"
let () = (printExp (algoritmoT exp)); print_endline ""
