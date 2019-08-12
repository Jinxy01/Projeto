
let exceptionRaised = ref false
let erro = ref "erro"
let iterator = ref 1
let exceptionFuntion (n,e,k) =
  let () = erro := e in
  let () = iterator := n in
  let () = exceptionRaised := true in
  k (-1)

let stateFunction (s,i,n) =
  Format.eprintf "Iteracao: %d, Valor n: %d@." i n;
  let _ = Scanf.scanf " %d" (fun x -> x) in
  s
 
let rec f i kExcep n kFunction =
  if i >= 5 then kFunction n else 
    match n with
    | 0 -> kExcep (i,"Zero",kFunction) 
    | n -> 
      if n < 0 then kExcep (i,"Número negativo",kFunction) else
        let newState = (fun fANF_0  -> kFunction (stateFunction(fANF_0,i,n))) in
        f (i + 1) kExcep (n - 1) newState 
;; 
 
let n = 10
let result = 
  (f (* Função *)
  0 (* iterator *)
  (fun e -> exceptionFuntion e) (* funcao de exceçao *)
  n (* n a avaliar *)
  (fun s -> s) (* funcao de retorno *)
  )
let () = 
  if !exceptionRaised 
    then Format.eprintf "Excecao: %s, na iteracao %d@." !erro !iterator
    else Format.eprintf "Resultado: %d@." result
