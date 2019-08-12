open Str
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Format 

(* --------------------------- Variáveis a utilizar ---------------------------------*)

(* Usada mais em baixo (applyOfLetInExpressions), auxiliar à construção de let ... in *)
let auxRefVarNameLetIn = ref 0

(* Auxiliar para perceber se estamos no caso function (temos de alterar para match para manter
estrutura de CPS) ou não e se é necessário adicionar argumento extra na transformação 
de function para match *) 
let changeFunctionToMatch = ref false

(* Perceber se devemos aplicar k (fun s -> s) à expressão. Por omissão sim, pois o primeiro
match case tendo a ser o que finaliza a função. Alterado para false, depois de ser aplicado a 
primeira vez, e apenas é transformado para true caso haja restructPexpLetInToCPS *)
let applyK = ref true

(* Variável para perceber se o argumento extra a adicionar (kFunction), resultante da aplicação CPS, 
é para aplicar ao let ... mais externo (onde está a extensão CPS) ou nos let ... dentro deste
Caso a extensão seja aplicada a um let não recursivo, então todos os lets dentro deste terão
o argumento adcionada. Caso a exntensão seja aplicada a um let recursivo então a adição de 
argumento extra só será a este let e não aos internos a este  *)
let applyExtraArgumentFromCPSTransf = ref false

(* Expressão fun s -> s *)
let funStoS = 
  (Nolabel,{pexp_desc = 
    Pexp_fun (Nolabel, None,
      {ppat_desc = Ppat_var {txt = "s"; loc = !(default_loc)}; ppat_attributes = []; ppat_loc = !(default_loc)},
      {pexp_desc = Pexp_ident {txt = Lident "s"; loc = !(default_loc)}; pexp_attributes = []; pexp_loc = !(default_loc;)}
    );
  pexp_attributes = [];
  pexp_loc = !(default_loc);
  })
;;

let hashTableFunctionName =  ref (Hashtbl.create 124678)
let () = Hashtbl.add (!hashTableFunctionName) "distr" "distr"
let () = Hashtbl.add (!hashTableFunctionName) "nnfc" "distr"
let () = Hashtbl.add (!hashTableFunctionName) "impFree" "distr"
let () = Hashtbl.add (!hashTableFunctionName) "cnfc" "distr"

(* --------------------- Comum a transformações ANF e CPS -------------------------- *)

(* Auxiliar para retornar corpo da função, sem argumentos; tb usada em transformação ANF *)
let rec returnFunctionIgnoringArguments currentFunction  = 
  match currentFunction with
    (* Ainda é argumento *)
    | {pexp_desc = Pexp_fun (_,_,_,expression); _} -> 
      returnFunctionIgnoringArguments expression
    (* Não é argumento, é corpo da função (o que é devolvido) *) 
    | _ as bodyFunction -> bodyFunction
;; 

(* Auxiliar à alteração de corpo de funções, sem avaliar argumentos *) 
let rec changeBodyFunctionIgnoringArguments currentFunction newBodyFunction  = 
  match currentFunction with
    | {pexp_desc = Pexp_fun (argLabel,expOption,pattern,expression); _} ->
        { 
          pexp_desc = Pexp_fun (argLabel,expOption,pattern, 
            changeBodyFunctionIgnoringArguments expression newBodyFunction);
          pexp_attributes = []; 
          pexp_loc = !(default_loc);
        } 
    (* Não é argumento, é corpo da função (substituir por novo corpo) *) 
    | _ -> newBodyFunction
;;

(* ----------------------------------- ANF ---------------------------------- *)

(* Introdução de expressões let in *)
let letInExpressionFunction functionName inExpression restBodyFunctionExp =
  (* Definição de nome de variável e atualização para próxima iteração *)
  let () = (auxRefVarNameLetIn := !(auxRefVarNameLetIn) - 1) in 
  let varName = functionName^"ANF_"^(string_of_int (!auxRefVarNameLetIn)) in
  (* Construção de let in *)
  {
    pexp_desc =
    Pexp_let (Nonrecursive,
      [{
        pvb_pat = {
          ppat_desc = Ppat_var {txt = varName; loc = !(default_loc)};
          ppat_attributes = [];
          ppat_loc = !(default_loc)
        };
        pvb_expr = inExpression;
        pvb_attributes = []; 
        pvb_loc = !(default_loc)
      }],
      restBodyFunctionExp
    );
    pexp_loc = !(default_loc);
    pexp_attributes = []; 
  }
;;

let makeVar name =
{
  pexp_desc = Pexp_ident {
    txt = Lident name; 
    loc = !(default_loc)};
  pexp_attributes = [];
  pexp_loc = !(default_loc)
}

(* Verificar lista de exp,VarName para verificar se deve alterar a expressão "exp" para
a variável associada *)
let rec alteraExp exp = function
  | [] -> exp
  | (el, varName)::rx -> 
    if el = exp then makeVar varName 
    else alteraExp exp rx

(* Altera o nome de variáveis para condizer com alterações de let in *)
let rec changeExpToMatchLetInVarName expression listExpLetIn =
  match expression with
  (* Aplicação particular *)
  | {pexp_desc = Pexp_apply (
        { pexp_desc = Pexp_ident {txt = Lident varName; loc = l}; 
          pexp_attributes = attIn; pexp_loc = locIn
        }, 
      applyList);
      pexp_attributes = att;
      pexp_loc = loc;
    } as applyExp -> 
    (* Verificar se applyExp pertence à lista de expressões a serem alteradas para nome
    de variável de let in criados, resultantes da transformação ANF *)
    let checkExp = alteraExp applyExp listExpLetIn in
    (* Caso seja, devolver resultado*)
    if applyExp <> checkExp 
      then checkExp 
    (* Caso contrário, avaliar applyList *)
    else 
    begin 
      let rec evaluateApplyList acc = function
        | [] -> List.rev acc
        | (label, exp) :: rx -> 
          (* Avaliar recursivamente a expressão *)
          let evaluateExp = changeExpToMatchLetInVarName exp listExpLetIn in
          (* Verificar se evaluateExp pertence à lista de expressões a serem alteradas (listLetIn) *)
          let checkExp = alteraExp evaluateExp listExpLetIn in  
          evaluateApplyList ((label,checkExp)::acc) rx
      in 
      let newApplyList = evaluateApplyList [] applyList in
      {
        pexp_desc = Pexp_apply (
          { pexp_desc = Pexp_ident {txt = Lident varName; loc = l}; 
            pexp_attributes = attIn; pexp_loc = locIn
          }, 
        newApplyList);
        pexp_attributes = att;
        pexp_loc = loc;
      } 
    end
  (* Aplicação genérica (pode ser apply de apply) *)
  | {pexp_desc = Pexp_apply (pexp, applyList); pexp_attributes = att; pexp_loc = loc} ->
    (* Avaliar recursivamente pexp e applyList *)
    let newPexp = changeExpToMatchLetInVarName pexp listExpLetIn in
    let rec evaluateApplyList acc = function
      | [] -> List.rev acc
      | (label, exp) :: rx -> 
        (* Avaliar recursivamente a expressão *)
        let evaluateExp = changeExpToMatchLetInVarName exp listExpLetIn in
        (* Verificar se evaluateExp pertence à lista de expressões a serem alteradas (listLetIn) *)
        let checkExp = alteraExp evaluateExp listExpLetIn in  
        evaluateApplyList ((label,checkExp)::acc) rx
    in 
    let newApplyList = evaluateApplyList [] applyList in
    {
      pexp_desc = Pexp_apply (newPexp, newApplyList); 
      pexp_attributes = att; pexp_loc = loc
    }    
  (* Podemos ter tuplos, dentro de aplicações e como tal devemos tratar destes casos *)
  | {pexp_desc = Pexp_tuple tuppleList; pexp_attributes = att; pexp_loc = loc} 
    ->

      (* Avaliar recursivamente expressões em tuppleList *)
      let rec evaluateTuppleList acc = function
        | [] -> List.rev acc
        | el::rx -> 
          let newExp = changeExpToMatchLetInVarName el listExpLetIn in
          evaluateTuppleList (newExp::acc) rx
      in
      let newTuppleList = evaluateTuppleList [] tuppleList in
      {
        pexp_desc = Pexp_tuple newTuppleList; 
        pexp_attributes = att; pexp_loc = loc
      }
  (* Nenhum dos casos contemplados -> Não é feita alteração *)
  | _ as otherExp -> otherExp

(* Promove aninhamento de let in's, provenientes da transformação para ANF das expressões 
da lista de chamadas recursivas (gatherListOfRecursiveCall) *)
let rec applyOfLetInExpressions expression functionName listLetIn = function
  | [] -> 
    (* Altera nome da expressão, de acordo com o definido em listLetIn (exp, nomeVariávelAssociada) *)
    changeExpToMatchLetInVarName expression listLetIn  
  | el :: rx -> 
    (* Alteração do nome da variável e atualização de identificador para pŕoxima iteração *)
    let varNameOfEl = functionName^"ANF_"^(string_of_int (!auxRefVarNameLetIn)) in
    let () = (auxRefVarNameLetIn := !auxRefVarNameLetIn + 1) in
    (* Chamada recursiva para tratar das restantes expressões da lista (aninhamento de let in)
    listLetIn, contém a expressão e o nome de variável que lhe ficará associada *)
    let newExp = applyOfLetInExpressions expression functionName ((el,varNameOfEl)::listLetIn) rx in
    (* Transfomação para let ... in *) 
    letInExpressionFunction functionName el newExp


(* A função que determina as alterações para let in em todos os casos considerados *)
let rec gatherListOfRecursiveCall expression functionName acc = 
  match expression with
  (* Aplicação particular *) 
  | {pexp_desc = Pexp_apply (
      {pexp_desc = Pexp_ident {txt = Lident varName; _}; _}, 
      applyList); _
    } as aplyExp ->  
      begin
        let rec avaliaApplyList acc = function
          | [] -> List.rev acc
          | (_,el)::rx ->
            (* Aplicação foi com o mesmo nome da função -> adicionar a "applyExp" a acc  
            para transformar em let in *)
            if varName = functionName then 
              (* Adicionar a hashtable *)
              let () = Hashtbl.add (!hashTableFunctionName) varName varName in
              aplyExp::acc 
            else 
              let newAcc = gatherListOfRecursiveCall el functionName acc in
              avaliaApplyList newAcc rx
        in 
        avaliaApplyList acc applyList 
      end
  (* Aplicação genérica *)
  | {pexp_desc = Pexp_apply (pexp, applyList); _ } -> 
      let newAcc = gatherListOfRecursiveCall pexp functionName acc in
      let rec avaliaApplyList acc = function
        | [] -> List.rev acc
        | (_,el)::rx ->
          let newAcc = gatherListOfRecursiveCall el functionName acc in
          avaliaApplyList newAcc rx
      in
      avaliaApplyList newAcc applyList
  (* Podemos ter tuplos, dentro de aplicações e como tal devemos tratar destes casos *)
  | {pexp_desc = Pexp_tuple tuppleList; _}
    ->
      let rec avaliaTuppleList acc = function
        | [] -> List.rev acc
        | el::rx -> 
          let newAcc = gatherListOfRecursiveCall el functionName acc in
          avaliaTuppleList newAcc rx
      in
      avaliaTuppleList acc tuppleList
  (* Não é nesta expressão que se encontra a chamada recursiva à função *) 
  | _ ->  acc


(* Auxiliar para acrescentar o matchArg quando é transformada = function em match ... with 
(durante ANF transformation) *)
let rec addMatchArgFunctionToMatchWithTransformation = function 
  | {pexp_desc = Pexp_fun (argLabel,expOption,pattern,expression); _} -> 
      {
        pexp_desc = Pexp_fun (argLabel,expOption,pattern, 
          addMatchArgFunctionToMatchWithTransformation expression);
        pexp_attributes = [];
        pexp_loc = !(default_loc);
    } 
  (* Não é argumento, é corpo da função => adicionar o argumento extra (matchArg) *) 
  | {pexp_desc = _; _ } as bodyFunction ->
      {
        pexp_desc = Pexp_fun (Nolabel,None,
          {ppat_desc = 
            Ppat_var {txt = "matchArg"; loc = !(default_loc)};
           ppat_attributes = [];
           ppat_loc = !(default_loc) 
          }, 
          bodyFunction);
        pexp_attributes = [];
        pexp_loc = !(default_loc);
      } 
;;

(* Transforma função em ANF *)
let rec changeFunctionToANF  expression functionName =
    (* Retorna o corpo da função, ignorando os argumentos *)
    let bodyFunction = returnFunctionIgnoringArguments expression in
    (* Por omissão, não teremos que alterar function para match *)
    let () = changeFunctionToMatch := false in
    (* Avalia corpo da função *)
    let newBody = evaluateExpressionANF functionName bodyFunction in
    let newExpression = 
      (if (!changeFunctionToMatch) then addMatchArgFunctionToMatchWithTransformation expression
        else expression) in
    let newFunction = changeBodyFunctionIgnoringArguments newExpression newBody in
    (* Alteração do corpo da função *)
    newFunction

(* Avaliação de diferentes tipos de expressão para alteração para ANF *)
and evaluateExpressionANF functionName = function
    (* Tratar de let in's de ITE, individualmente *)
    | {pexp_desc = Pexp_ifthenelse (ifExp, thenExp, Some(elseExp)); 
      pexp_attributes = att; pexp_loc = loc } ->
        (* avaliar ITE aninhados *)
        let letInListIfExp = evaluateExpressionANF functionName ifExp in
        let letInListThenExp = evaluateExpressionANF functionName thenExp in
        let letInListElseExp = evaluateExpressionANF functionName elseExp in
        {
          pexp_desc = Pexp_ifthenelse (
          letInListIfExp, 
          letInListThenExp, 
          Some(letInListElseExp)); 
          pexp_attributes = att; pexp_loc = loc 
        }
    (* Tratar de let in's de IT, individualmente *)
    | {pexp_desc = Pexp_ifthenelse (ifExp, thenExp, None); 
      pexp_attributes = att; pexp_loc = loc } ->
        (* avaliar ITE aninhados *)
        let letInListIfExp = evaluateExpressionANF functionName ifExp in
        let letInListThenExp = evaluateExpressionANF functionName thenExp in
        {
          pexp_desc = Pexp_ifthenelse (
          letInListIfExp, 
          letInListThenExp,  
          None); 
          pexp_attributes = att; pexp_loc = loc 
        }
    (* function = |.. -> .. *)
    | {pexp_desc = Pexp_function(listMatchCases); 
      pexp_attributes = att; pexp_loc = loc } ->
      (* Converter todas as function to match ... with *)
        let () = changeFunctionToMatch := true in
        let matchArg = {pexp_desc = Pexp_ident 
          {txt = Lident "matchArg"; loc = !(default_loc)}; 
          pexp_attributes = []; 
          pexp_loc = !(default_loc)
        } in
        {pexp_desc = (Pexp_match (matchArg, (changeMatchCasesANF listMatchCases functionName [])));
          pexp_attributes = att; 
          pexp_loc = loc
        }
      (* Match ... with aninhados  *)
    | {pexp_desc = Pexp_match (matchArg ,listMatchCases); 
      pexp_attributes = att; pexp_loc = loc } ->
        let letInListMatch = changeMatchCasesANF listMatchCases functionName [] in
        {
          pexp_desc = Pexp_match (
          matchArg, 
          letInListMatch); 
          pexp_attributes = att; pexp_loc = loc 
        }
    (* Caso de let in *)
    | {pexp_desc = Pexp_let (recLabel,
        pvbPattList,  
        nextLetInexp); 
        pexp_attributes = pexpAtt; pexp_loc = pexpLoc
      } ->
        begin
          (* Transformação para ANF *)
          let newpvbPattList = changePvbPatListANF functionName [] pvbPattList in
          let newNextLetInExp = evaluateExpressionANF functionName nextLetInexp in 
          {pexp_desc = Pexp_let (recLabel,
              newpvbPattList, 
              newNextLetInExp); 
          pexp_attributes = pexpAtt; pexp_loc = pexpLoc
          } 
        end 
    (* Sequencia  *) 
    | {pexp_desc = Pexp_sequence(exp1, exp2);
        pexp_attributes = pAtt; pexp_loc = pLoc 
      } -> 
        let letInListExp1 = evaluateExpressionANF functionName exp1 in
        let letInListExp2 = evaluateExpressionANF functionName exp2 in
        { 
          pexp_desc = Pexp_sequence (
            letInListExp1, 
            letInListExp2
          ); 
          pexp_attributes = pAtt; pexp_loc = pLoc 
        }
    (* try ... with Not_found -> *)
    | {pexp_desc = Pexp_try(exp, withList);
        pexp_attributes = pAtt; pexp_loc = pLoc 
      } ->
        let letInListExp = evaluateExpressionANF functionName exp in
        (* Not_found tem a estrutura de matchCases *)
        let letInListWithList = changeMatchCasesANF withList functionName [] in
        {
          pexp_desc = Pexp_try(letInListExp, letInListWithList);
          pexp_attributes = pAtt; pexp_loc = pLoc 
        }
    (* Caso de :: (e.g) usado em listas (List) ou N(...) em árvores *)
    | {pexp_desc = Pexp_construct(lident, Some(exp));
        pexp_attributes = pAtt; pexp_loc = pLoc 
      } ->
        let newExp = evaluateExpressionANF functionName exp in
        {
          pexp_desc = Pexp_construct(lident, Some(newExp)); (* irá para Pexp_tuple *)
          pexp_attributes = pAtt; pexp_loc = pLoc 
        }
    (* Caso de r :: f a (e.g, tuple -> r, f a), usado em listas (List) *)
    | {pexp_desc = Pexp_tuple(tupleList);
        pexp_attributes = pAtt; pexp_loc = pLoc 
      } ->
        let rec evaluateTupleList acc = function
          | [] -> List.rev acc
          | el::rx -> 
            let evaluateEl = evaluateExpressionANF functionName el in
            evaluateTupleList (evaluateEl::acc) rx
        in         
        let newTupleList = evaluateTupleList [] tupleList in
        {
          pexp_desc = Pexp_tuple(newTupleList);
          pexp_attributes = pAtt; pexp_loc = pLoc 
        }
    (* E.g let t : int = ... *)
    | {pexp_desc = Pexp_constraint(exp,coreType);
        pexp_attributes = pAtt; pexp_loc = pLoc 
      } ->
        let letInExp = evaluateExpressionANF functionName exp in
        {
          pexp_desc = Pexp_constraint(letInExp,coreType);
          pexp_attributes = pAtt; pexp_loc = pLoc 
        }
    (* Outros casos (aplicação/base). Não vamos querer tratar do caso da aplicação em particular,
    senão ficariamos com let .. in dentro de aplicações. Assim vamos "olhar em frente" de aplicações
    e expressões simples, recolher a lista de aplicações a serem transformadas em let .. in e
    aninha-los, ficando "exp" no final do ultimo let in *)
    | _ as exp -> 
        (* Recolher lista de aplicações para transformar em let in *) 
        let letInList = gatherListOfRecursiveCall exp functionName [] in
        (* Aplicar transformações *)
        let newExp = applyOfLetInExpressions exp functionName [] (List.rev letInList) in
        newExp

(* Tratar de matchCases e withCases (caso de try ... with Not_found, e.g.) *)
and changeMatchCasesANF matchCasesList functionName acc =
  match matchCasesList with 
  | [] -> List.rev acc
  | {pc_lhs = pcLhs ; pc_guard = pcGuard;
    pc_rhs =  matchContent} :: rx -> 
      let newMatchCase = 
        {
          pc_lhs = pcLhs; 
          pc_guard = pcGuard;
          pc_rhs =  evaluateExpressionANF functionName matchContent
        } 
      in
      changeMatchCasesANF rx functionName (newMatchCase::acc)

(* Avalia pvbPatList: conteudo dentro de let in ou let and sucessivos *)
and changePvbPatListANF functionNameExternal acc = function
  | [] -> List.rev acc
  | { pvb_pat = {ppat_desc = Ppat_var {txt = functionName; loc = loc};
      ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
      pvb_expr = expression; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
    } ::rx ->  
      (* Transformação para ANF *)
      (* Retirar argumentos, para não interferir com avaliação *)
      let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
      let pvbExpAnf = evaluateExpressionANF functionName pvbExpWithoutArguments in
       (* Repor argumentos *)
      let pvbExpAnfWithArguments = changeBodyFunctionIgnoringArguments expression pvbExpAnf in
      let tempAnf =
      {
          pvb_pat = {ppat_desc = Ppat_var {txt = functionName; loc = loc};
          ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
          pvb_expr = pvbExpAnfWithArguments; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
      } 
      in 
      changePvbPatListANF functionNameExternal (tempAnf::acc) rx
  (* E.g let () = ... *)  
  | { pvb_pat = {ppat_desc = Ppat_construct ({txt = Lident varName; loc = loc}, option);
      ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
      pvb_expr = expression; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
    } ::rx -> 
      (* Transformação para ANF *)
      let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
      let pvbExpAnf = evaluateExpressionANF varName pvbExpWithoutArguments  in
      let pvbExpAnfWithArguments = changeBodyFunctionIgnoringArguments expression pvbExpAnf in
      let tempAnf =
      {
          pvb_pat = {ppat_desc = Ppat_construct ({txt = Lident varName; loc = loc}, option);
          ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
          pvb_expr = pvbExpAnfWithArguments; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
      } 
      in 
      changePvbPatListANF functionNameExternal (tempAnf::acc) rx
  (* E.g let (x,y) = ... *)
  | { pvb_pat = {ppat_desc = Ppat_tuple (tuppleList);
      ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
      pvb_expr = expression; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
    } ::rx -> 
      let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
      let pvbExpAnf = evaluateExpressionANF functionNameExternal pvbExpWithoutArguments  in
      let pvbExpAnfWithArguments = changeBodyFunctionIgnoringArguments expression pvbExpAnf in
      let tempAnf =
      {
        pvb_pat = {ppat_desc =  Ppat_tuple (tuppleList);
        ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
        pvb_expr = pvbExpAnfWithArguments; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
      }   
      in
      changePvbPatListANF functionNameExternal (tempAnf::acc) rx
  (* Não promover transformação *)
  | exp::rx -> changePvbPatListANF functionNameExternal (exp::acc) rx  


(* ----------------------------------- CPS ---------------------------------- *)

(* Função auxiliar que nos permite perceber se a applicação a avaliar, oriunda de let in, foi criada 
pelo ppx (transformação para ANF) e portanto terá de ser reestruturado para CPS, ou é uma aplicação 
criada pelo utilizador. A maneira de avaliar tal é verifcar se o nome da variavel, em let varName = ...
tem a estrutura de uma variável criada por ppx (functionName+ANF_#id) *)
let checkIfApplicationComesFromANFTransformation varNamePatt =
  match varNamePatt with
  | {ppat_desc = Ppat_var {txt = varName; _}; _} -> 
  begin
    (* Avalia se varName contém "ANF_", de Str *)
    let regExpAnf = regexp_string "ANF_" in
    (* Procurar em varName ANF_ a partir da posição 0 *)
    try let _ = search_forward regExpAnf varName 0 in true
    with Not_found -> false
  end
  | _ ->  false 
;;

(* Introducao de argumento exta, kFunction (fun s -> s), ao argumentos da função *)
let rec addArgumentToFunction = function
  (* Ainda é argumento. Avaliar recursivamente o próximo argumento *)
  | {pexp_desc = Pexp_fun (argLabel,expOption,pattern,expression); 
     pexp_attributes = att; pexp_loc = loc} -> 
      {
        pexp_desc = Pexp_fun (argLabel,expOption,pattern, 
          addArgumentToFunction expression);
        pexp_attributes = att; pexp_loc = loc
      } 
  (* Não é argumento, é corpo da função. Adicionar o argumento kFunction (fun s -> s) *) 
  | _ as bodyFunction -> 
      {
        (* Adição do argumento kFunction (fun s -> s) ao conjunto de argumentos da função *)
        pexp_desc =
          Pexp_fun (Nolabel, None,
            {
              ppat_desc = Ppat_var {
                txt = "kFunction"; loc = !(default_loc)
              };
              ppat_attributes = [];
              ppat_loc = !(default_loc);
            }, 
          bodyFunction
        );
        pexp_attributes = [];
        pexp_loc = !(default_loc);
      } 
;;

let applyKExp expression = 
  {pexp_desc = Pexp_apply (
    {pexp_desc = Pexp_ident {txt = Lident "kFunction"; loc = !(default_loc)}; 
    pexp_loc = !(default_loc); 
    pexp_attributes = []
    },
    (* A expressão que já existia, aparece aplicada a k *)
    [(Nolabel, expression)]
    );
  pexp_loc = !(default_loc); 
  pexp_attributes = [];
  }
;;
 
(* Auxiliar para percorrer todos os argumentos da aplicaçao da chamada recursiva em let in, chegar
ao fim da lista e adicionar o fun (varName -> ...), chamando depois changeExpressionLetInsToCPSStyleAux *)
let rec restructPexpLetInToCPS acc varName restExpression = function
  | [] ->
    let aux = changeExpressionLetInsToCPSStyle restExpression in
    let aux2 =(Nolabel, {
      pexp_desc = 
        Pexp_fun (Nolabel, None, varName, (aux));
      pexp_attributes = [];
      pexp_loc = !(default_loc) 
      })
    in 
    let listLetIns = List.rev (aux2::acc) in
    listLetIns
  | (label, exp)::rx ->  restructPexpLetInToCPS ((label, exp)::acc)  varName restExpression rx

(* Converter todos os let in's  em .... (fun ... -> ), CPS style
  É garantido que, ao chegar a esta função, a expressão já está em ANF *)
and changeExpressionLetInsToCPSStyle = function
    (* ITE, if não é aplicado CPS *)
    | {pexp_desc = Pexp_ifthenelse (ifExp, thenExp, Some(elseExp)); 
        pexp_attributes = att; pexp_loc = loc} 
        -> 
        (* Caso particular *)
        let () = applyK := false in
        let newIfExp = changeExpressionLetInsToCPSStyle ifExp in
        let () = applyK := true in
        let newThenExp = changeExpressionLetInsToCPSStyle thenExp in
        let newElseExp = changeExpressionLetInsToCPSStyle elseExp in
        {
          pexp_desc = Pexp_ifthenelse (
            newIfExp, 
            newThenExp, 
            Some(newElseExp)
            );  
            pexp_attributes = att; pexp_loc = loc
        }
    (* IT *)
    | {pexp_desc = Pexp_ifthenelse (ifExp, thenExp, None); 
        pexp_attributes = att; pexp_loc = loc} 
        -> 
        let newIfExp = changeExpressionLetInsToCPSStyle ifExp in
        let newThenExp = changeExpressionLetInsToCPSStyle thenExp in
        {
          pexp_desc = Pexp_ifthenelse (
            newIfExp, 
            newThenExp, 
            None
            ); 
            pexp_attributes = att; pexp_loc = loc
        }
    (* function = ... *)
    | {pexp_desc = (Pexp_function (matchCasesList));
        pexp_attributes = att; 
        pexp_loc = loc
      } -> 
        let newMatchCaseList = changeMatchCasesCPS matchCasesList [] in
        {
          pexp_desc = (Pexp_function (
            newMatchCaseList));
          pexp_attributes = att; 
          pexp_loc = loc
        }
    (* Match case aninhados *)
    | {pexp_desc = (Pexp_match (matchArg, matchCasesList));
        pexp_attributes = att; 
        pexp_loc = loc
      } ->
        let newMatchCaseList = changeMatchCasesCPS matchCasesList [] in 
        {
          pexp_desc = (Pexp_match 
            (matchArg,  
            newMatchCaseList));
          pexp_attributes = att; 
          pexp_loc = loc
        } 
    (* Avaliar sequência *)
    | {pexp_desc = (Pexp_sequence (exp1, exp2)); 
       pexp_attributes = att; pexp_loc = loc}
       -> 
        let newExp1 = changeExpressionLetInsToCPSStyle exp1 in
        let newExp2 = changeExpressionLetInsToCPSStyle exp2 in  
        {
          pexp_desc = (Pexp_sequence (
            newExp1, 
            newExp2)
          ); 
          pexp_attributes = att; 
          pexp_loc = loc
        }
    (* Avaliar try ... with Not_found (e.g) *)
    | {pexp_desc = (Pexp_try (exp, withList)); 
       pexp_attributes = att; pexp_loc = loc}
       ->
        let newExp = changeExpressionLetInsToCPSStyle exp in
        let newWithList = changeMatchCasesCPS withList [] in 
        {
          pexp_desc = (Pexp_try (
            newExp,
            (* withList tem mesma estrutura de mathCase *) 
            newWithList)
          ); 
          pexp_attributes = att; 
          pexp_loc = loc
        }
    (* Caso de :: (e.g), usado em listas (List) *)
    | {pexp_desc = Pexp_construct(lident, Some(exp));
        pexp_attributes = pAtt; pexp_loc = pLoc 
      } ->
        let newExp = changeExpressionLetInsToCPSStyle exp in
        {
          pexp_desc = Pexp_construct(lident, Some(newExp));
          pexp_attributes = pAtt; pexp_loc = pLoc 
        }
    (* Caso de r :: f a (e.g, tuple -> r, f a), usado em listas (List) *)
    | {pexp_desc = Pexp_tuple(tupleList);
      pexp_attributes = pAtt; pexp_loc = pLoc 
      } ->
        let rec evaluateTupleList acc = function
          | [] -> List.rev acc
          | el::rx -> 
            let evaluateEl = changeExpressionLetInsToCPSStyle el in
            evaluateTupleList (evaluateEl::acc) rx
        in         
        let newTupleList = evaluateTupleList [] tupleList in
        {
          pexp_desc = Pexp_tuple(newTupleList);
          pexp_attributes = pAtt; pexp_loc = pLoc 
        } 
    (* Caso particular de let: Onde será feita a alteração de ANF para CPS *)
    | {pexp_desc = Pexp_let (recLabel,
        { pvb_pat = {ppat_desc = Ppat_var {txt = functionName; loc = l};
          ppat_attributes = ppaAtt; ppat_loc = ppatLoc} as varNamePatt; 
          pvb_expr = 
            {pexp_desc = Pexp_apply (pexp, list); 
             pexp_attributes = att; pexp_loc = loc} as expression; 
             pvb_attributes = pvbAtt; pvb_loc = pvbLoc
            } ::rx,   
        nextLetInexp); 
        pexp_attributes = pexpAtt; pexp_loc = pexpLoc 
      } ->
          (* Applicação provém de um let in, oriundo de transformação ANF. 
          Transformar let in em aplicação, com reestruturação da expressão *) 
          if (checkIfApplicationComesFromANFTransformation varNamePatt) then
              let temp = (restructPexpLetInToCPS [] varNamePatt nextLetInexp list) in
              (*let newTemp = {pexp_desc = Pexp_apply (
                {pexp_desc = Pexp_ident {txt = Lident "kFunction"; loc = !(default_loc)}; 
                pexp_loc = !(default_loc); 
                pexp_attributes = []
                },
                (* A expressão que já existia, aparece aplicada a k *)
                [(Nolabel, temp)]
                );
                  pexp_loc = !(default_loc); 
                  pexp_attributes = [];
              } in*)
              {pexp_desc = Pexp_apply  
                (pexp,
                (* Reestrutura ANF para CPS *) 
                temp
                );  
              pexp_attributes = att; 
              pexp_loc = loc} 
          else
            (* Avaliar de forma genérica *)
            let newpvbPattList = changePvbPatListCPS [] rx  in
            let newNextLetInExp = changeExpressionLetInsToCPSStyle nextLetInexp in
            let newPvbExp = changeExpressionLetInsToCPSStyle expression in
            {pexp_desc = Pexp_let (recLabel,
              { pvb_pat = {ppat_desc = Ppat_var {txt = functionName; loc = l};
                ppat_attributes = ppaAtt; ppat_loc = ppatLoc} ; 
                pvb_expr = 
                  newPvbExp; 
                  pvb_attributes = pvbAtt; pvb_loc = pvbLoc
                  } ::newpvbPattList,   
              newNextLetInExp); 
              pexp_attributes = pexpAtt; pexp_loc = pexpLoc 
            }
    (* Let in, genericamente tratado *)
    | {pexp_desc = Pexp_let (recLabel,
        pvbPattList,  
        nextLetInexp); 
        pexp_attributes = pexpAtt; pexp_loc = pexpLoc 
      } ->
        let newpvbPattList = changePvbPatListCPS [] pvbPattList in
        let newNextLetInExp = changeExpressionLetInsToCPSStyle nextLetInexp in
        {
          pexp_desc = Pexp_let (recLabel,
          newpvbPattList,  
          newNextLetInExp); 
          pexp_attributes = pexpAtt; pexp_loc = pexpLoc 
        }
    | {pexp_desc = Pexp_apply (exp, applyList); 
      pexp_attributes = att; pexp_loc = loc}
        -> 
          (* Verificar se devemos aplicar kFunction (fun s -> s). Para tal usaremos uma temporaria
          interna. Para evitar que aplicações recursivas sejam todas aplicadas a kFunction,
          alteraremos a variavel global para false *)
          let inApplyK = !applyK in
          let () = applyK := false in
          let rec evaluateApplyList acc = function
            | [] -> 
                begin
                  (* Aplicação de argumento extra à função caso esta tenha sido transformada em CPS *)
                  try  
                    let _ = Hashtbl.find (!hashTableFunctionName) "nnfc" in
                    let newList = funStoS:: acc in 
                    List.rev newList
                  with Not_found -> List.rev acc
                  (* List.rev acc *)
                end
            | (label, exp) :: rx -> 
              (* Avaliar recursivamente a expressão *)
              let evaluateExp = changeExpressionLetInsToCPSStyle exp in
              evaluateApplyList ((label,evaluateExp)::acc) rx
          in 
          let newApplyList = evaluateApplyList [] applyList in
          let newExp = changeExpressionLetInsToCPSStyle exp in 
          let temp = {
            pexp_desc = Pexp_apply (newExp, newApplyList); 
            pexp_attributes = att; pexp_loc = loc
          } in
          (* Garantir que a aplicação de kFunction ocorre quando a função deste tem o 
          argumento extra. "not" (explicação no topo da página) para evitar que quando aplico
          %CPS a uma função não recursiva, instruções antes das funções recursivas dentro desta
          não tenham kFunction aplicado  *)
          let () = applyK := true in
          let returnExp = 
            (if (inApplyK && not !applyExtraArgumentFromCPSTransf) 
            then applyKExp temp 
            else temp) in 
          returnExp
    (* Identificador, nome de variável *)
    | { pexp_desc = Pexp_ident (_); _
      } as identExp -> 
        (* Verificar se devemos aplicar kFunction (fun s -> s) *)
        let returnExp = 
          (if (!applyK ) 
          then applyKExp identExp 
          else identExp) in
        returnExp
    (* Constante *)
    | { pexp_desc = Pexp_constant (_); _
      } as constExp -> 
        (* Verificar se devemos aplicar kFunction (fun s -> s) *)
        let returnExp = 
          (if (!applyK)
          then applyKExp constExp 
          else constExp) in
        returnExp
    (* Construtor sem "parametros". E.g: false, true *)
    | { pexp_desc = Pexp_construct (_, None); _
      } as constructExp ->
        (* Verificar se devemos aplicar kFunction (fun s -> s) *)
        let returnExp = (if !applyK then applyKExp constructExp else constructExp) in
        returnExp
     (* E.g let t : int = ... *)
    | {pexp_desc = Pexp_constraint (exp, coreType); 
       pexp_attributes = att; pexp_loc = loc} ->
        let newExp = changeExpressionLetInsToCPSStyle exp in
        (*{
          pexp_desc = Pexp_constraint (newExp, coreType); 
          pexp_attributes = att; pexp_loc = loc
        }*) newExp
    (* Expressões não consideradas *)               
    | _ as expression -> 
      eprintf "Expression: %s@." (Pprintast.string_of_expression expression); 
      expression

(* Tratar de matchCases e withCases (caso de try ... with Not_found, e.g.) *)
and changeMatchCasesCPS matchCasesList acc =
  match matchCasesList with 
  | [] -> List.rev acc
  | {pc_lhs = pcLhs ; pc_guard = pcGuard;
    pc_rhs =  matchContent} :: rx -> 
      let newMatchCase = 
        {
          pc_lhs = pcLhs; 
          pc_guard = pcGuard;
          pc_rhs =  changeExpressionLetInsToCPSStyle matchContent
        } 
      in
      changeMatchCasesCPS rx (newMatchCase::acc)

(* Avalia pvbPatList: conteudo dentro de let in ou let and sucessivos *)
and changePvbPatListCPS acc = function
  | [] -> List.rev acc 
  | { pvb_pat = {ppat_desc = Ppat_var {txt = functionName; loc = loc};
      ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
      pvb_expr = expression; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
    } ::rx ->  
      (* Retirar argumentos, para não interferir com avaliação *)
      let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
      (* Transformação para CPS *)
      let pvbExpCPS = changeExpressionLetInsToCPSStyle pvbExpWithoutArguments in
      (* Retirar argumentos, para não interferir com avaliação *)
      let pvbExpCPSWithArguments = changeBodyFunctionIgnoringArguments expression pvbExpCPS in
      (* Verificar se devemos aplicar o argumento extra, resultante da transformação CPS *)
      let pvbExpCPSWithArguments =
        if (!applyExtraArgumentFromCPSTransf) then addArgumentToFunction pvbExpCPSWithArguments
        else pvbExpCPSWithArguments
      in
      let tempCPS = 
      {
          pvb_pat = {ppat_desc = Ppat_var {txt = functionName; loc = loc};
          ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
          pvb_expr = pvbExpCPSWithArguments; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
      } 
      in 
      changePvbPatListCPS (tempCPS::acc) rx
  (* E.g Let () = ... *)  
  | { pvb_pat = {ppat_desc = Ppat_construct ({txt = Lident varName; loc = loc}, option);
      ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
      pvb_expr = expression; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
    } ::rx -> 
      (* Retirar argumentos, para não interferir com avaliação *)
      let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
      (* Transformação para CPS *)
      let pvbExpCPS = changeExpressionLetInsToCPSStyle pvbExpWithoutArguments in
      (* Retirar argumentos, para não interferir com avaliação *)
      let pvbExpCPSWithArguments = changeBodyFunctionIgnoringArguments expression pvbExpCPS in
       let pvbExpCPSWithArguments =
        if (!applyExtraArgumentFromCPSTransf) then addArgumentToFunction pvbExpCPSWithArguments
        else pvbExpCPSWithArguments
      in
      let tempCPS =
      {
          pvb_pat = {ppat_desc = Ppat_construct ({txt = Lident varName; loc = loc}, option);
          ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
          pvb_expr = pvbExpCPSWithArguments; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
      } 
      in 
      changePvbPatListCPS (tempCPS::acc) rx
  | { pvb_pat = {ppat_desc = Ppat_tuple (tuppleList);
      ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
      pvb_expr = expression; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
    } ::rx -> 
      (* Retirar argumentos, para não interferir com avaliação *)
      let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
      (* Transformação para CPS *)
      let pvbExpCPS = changeExpressionLetInsToCPSStyle pvbExpWithoutArguments in
      (* Retirar argumentos, para não interferir com avaliação *)
      let pvbExpCPSWithArguments = changeBodyFunctionIgnoringArguments expression pvbExpCPS in
       let pvbExpCPSWithArguments =
        if (!applyExtraArgumentFromCPSTransf) then addArgumentToFunction pvbExpCPSWithArguments
        else pvbExpCPSWithArguments
      in
      let tempCPS = 
      {
        pvb_pat = {ppat_desc =  Ppat_tuple (tuppleList);
        ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
        pvb_expr = pvbExpCPSWithArguments; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
      }   
      in
      changePvbPatListCPS (tempCPS::acc) rx
  (* Não promover transformação *)
  | exp::rx -> changePvbPatListCPS (exp::acc) rx  

(* Transforma função em estilo CPS (assumindo que esta está em ANF) *)
and changeFunctionToCPS expression =
  (* Resetar variável de aplicação de kFunction (fun s -> s) *)
   let () = applyK := true in
  (* Obter corpo da função e passar para CPS *)
  let newBody = returnFunctionIgnoringArguments expression in
  let newExpCPS = changeExpressionLetInsToCPSStyle newBody in
  (* Criar nova função, em estilo CPS, comos seus argumentos re-incorporados *)
  let newFunctionCPS = changeBodyFunctionIgnoringArguments expression newExpCPS in
  (* Adição de argumento extra kFuntion (fun s -> s), aos parametros da função
  Verificar se terá que ser adicionada ou não. Aqui, se a variável for verdadeira
  significa que adicionamos argumento extra aos lets dentro da função, logo não é
  suposto adicionar neste let *)
  let newExp =
      if (!applyExtraArgumentFromCPSTransf) then newFunctionCPS
      else addArgumentToFunction newFunctionCPS
  in
  newExp

(* ------------------ Avaliar expressões para adição de argumento extra,
                  caso seja necessário (resultante da transformação CPS) ----------------------- *)

(* Avaliar expressões para verificar se é necessário adicionar argumento extra (kFunctio : fun 
(s -> s)), resultantes da transformação para CPS *)
let rec checkIfExpressionNeedsExtraArgument expression hashTableFunctionName =
  (* Retorna o corpo da função, ignorando os argumentos *)
  let bodyFunction = returnFunctionIgnoringArguments expression in
  (* Avalia corpo da função *)
  let newBody = addExtraArgumentToCPSTransformedFunction hashTableFunctionName bodyFunction in
  let newFunction = changeBodyFunctionIgnoringArguments expression newBody in
  (* Alteração do corpo da função *)
  newFunction

and addExtraArgumentToCPSTransformedFunction hashTableFunctionName = function
    (* Tratar de let in's de ITE, individualmente *)
    | {pexp_desc = Pexp_ifthenelse (ifExp, thenExp, Some(elseExp)); 
      pexp_attributes = att; pexp_loc = loc } ->
        (* avaliar ITE aninhados *)
        let letInListIfExp = addExtraArgumentToCPSTransformedFunction hashTableFunctionName ifExp in
        let letInListThenExp = addExtraArgumentToCPSTransformedFunction hashTableFunctionName thenExp in
        let letInListElseExp = addExtraArgumentToCPSTransformedFunction hashTableFunctionName elseExp in
        {
          pexp_desc = Pexp_ifthenelse (
          letInListIfExp, 
          letInListThenExp, 
          Some(letInListElseExp)); 
          pexp_attributes = att; pexp_loc = loc 
        }
    (* Tratar de let in's de IT, individualmente *)
    | {pexp_desc = Pexp_ifthenelse (ifExp, thenExp, None); 
      pexp_attributes = att; pexp_loc = loc } ->
        (* avaliar ITE aninhados *)
        let letInListIfExp = addExtraArgumentToCPSTransformedFunction hashTableFunctionName ifExp in
        let letInListThenExp = addExtraArgumentToCPSTransformedFunction hashTableFunctionName thenExp in
        {
          pexp_desc = Pexp_ifthenelse (
          letInListIfExp, 
          letInListThenExp,  
          None); 
          pexp_attributes = att; pexp_loc = loc 
        }
    (* function = |.. -> .. *)
    | {pexp_desc = Pexp_function(listMatchCases); 
      pexp_attributes = att; pexp_loc = loc } ->
      (* Converter todas as function to match ... with *)
        let newListMatchCases = 
          checkMatchCasesForExtraArgumentAdd listMatchCases [] hashTableFunctionName in
        {
          pexp_desc = Pexp_function(newListMatchCases); 
          pexp_attributes = att; pexp_loc = loc 
        }
      (* Match ... with aninhados  *)
    | {pexp_desc = Pexp_match (matchArg ,listMatchCases); 
      pexp_attributes = att; pexp_loc = loc } ->
        let newListMatchCases = 
          checkMatchCasesForExtraArgumentAdd listMatchCases [] hashTableFunctionName in
        {
          pexp_desc = Pexp_match (
          matchArg, 
          newListMatchCases); 
          pexp_attributes = att; pexp_loc = loc 
        }
    (* Caso de let in *)
    | {pexp_desc = Pexp_let (recLabel,
        pvbPattList,  
        nextLetInexp); 
        pexp_attributes = pexpAtt; pexp_loc = pexpLoc
      } ->
        begin
            (* Transformação para ANF *)
            let newpvbPattList = 
              evaluateValueBindingListForExtraArgumentAdd hashTableFunctionName [] pvbPattList in
            let newNextLetInExp = 
              addExtraArgumentToCPSTransformedFunction hashTableFunctionName nextLetInexp in
            (* Transformação temporária para bater certo com o protótipo de changeFunctionToCPS *)
             {pexp_desc = Pexp_let (recLabel,
                newpvbPattList, 
                newNextLetInExp); 
            pexp_attributes = pexpAtt; pexp_loc = pexpLoc
            } 
        end 
    (* Sequencia  *) 
    | {pexp_desc = Pexp_sequence(exp1, exp2);
        pexp_attributes = pAtt; pexp_loc = pLoc 
      } -> 
        let letInListExp1 = addExtraArgumentToCPSTransformedFunction hashTableFunctionName exp1 in
        let letInListExp2 = addExtraArgumentToCPSTransformedFunction hashTableFunctionName exp2 in
        { 
          pexp_desc = Pexp_sequence (
            letInListExp1, 
            letInListExp2
          ); 
          pexp_attributes = pAtt; pexp_loc = pLoc 
        }
    (* try ... with Not_found -> *)
    | {pexp_desc = Pexp_try(exp, withList);
        pexp_attributes = pAtt; pexp_loc = pLoc 
      } ->
        let letInListExp = addExtraArgumentToCPSTransformedFunction hashTableFunctionName exp in
        (* Not_found tem a estrutura de matchCases *)
        let letInListWithList = 
          checkMatchCasesForExtraArgumentAdd withList [] hashTableFunctionName in
        {
          pexp_desc = Pexp_try(letInListExp, letInListWithList);
          pexp_attributes = pAtt; pexp_loc = pLoc 
        }
    (* Caso de :: (e.g) usado em listas (List) ou N(...) em árvores *)
    | {pexp_desc = Pexp_construct(lident, Some(exp));
        pexp_attributes = pAtt; pexp_loc = pLoc 
      } ->
        let newExp = addExtraArgumentToCPSTransformedFunction hashTableFunctionName exp in
        {
          pexp_desc = Pexp_construct(lident, Some(newExp)); (* irá para Pexp_tuple *)
          pexp_attributes = pAtt; pexp_loc = pLoc 
        }
    (* Caso de r :: f a (e.g, tuple -> r, f a), usado em listas (List) *)
    | {pexp_desc = Pexp_tuple(tupleList);
        pexp_attributes = pAtt; pexp_loc = pLoc 
      } ->
        let rec evaluateTupleList acc = function
          | [] -> List.rev acc
          | el::rx -> 
            let evaluateEl = addExtraArgumentToCPSTransformedFunction hashTableFunctionName el in
            evaluateTupleList (evaluateEl::acc) rx
        in         
        let newTupleList = evaluateTupleList [] tupleList in
        {
          pexp_desc = Pexp_tuple(newTupleList);
          pexp_attributes = pAtt; pexp_loc = pLoc 
        }
    (* Aplicação particular *) 
    | {pexp_desc = Pexp_apply (
        {
          pexp_desc = Pexp_ident {txt = Lident varName; loc = l};
          pexp_attributes = attIn; pexp_loc = locIn
        }, 
        applyList);
        pexp_attributes = att;
        pexp_loc = loc
      } ->
        begin
          let rec evaluateListApply acc = function
            | [] -> begin
                try  
                  let _ = Hashtbl.find hashTableFunctionName varName in
                  let newList = funStoS:: acc in 
                  List.rev newList
                with Not_found -> List.rev acc
              end
            | (label,exp) :: rx ->
              let evaluatedExp = addExtraArgumentToCPSTransformedFunction hashTableFunctionName exp in
              evaluateListApply ((label,evaluatedExp)::acc) rx
          in
          let newListApply = evaluateListApply [] applyList in
              {pexp_desc = Pexp_apply (
                {
                  pexp_desc = Pexp_ident {txt = Lident varName; loc = l};
                  pexp_attributes = attIn; pexp_loc = locIn
                }, 
                newListApply);
                pexp_attributes = att;
                pexp_loc = loc
              }
        end
    (* Aplicação genérica *)
    | {pexp_desc = Pexp_apply (pexp, applyList); 
      pexp_attributes = att; pexp_loc = loc} -> 
        let newPexp = addExtraArgumentToCPSTransformedFunction hashTableFunctionName pexp in
        let rec evaluateApplyList acc = function
          | [] -> List.rev acc
          | (label,el)::rx -> 
            let newEl = addExtraArgumentToCPSTransformedFunction hashTableFunctionName el in
            evaluateApplyList ((label,newEl)::acc) rx
        in
        let newApplyList = evaluateApplyList [] applyList in
        {
          pexp_desc = Pexp_apply (newPexp, newApplyList); 
          pexp_attributes = att; pexp_loc = loc
        }
    (* Outros casos base *)
    | _ as exp -> exp

(* Tratar de matchCases e withCases (caso de try ... with Not_found, e.g.) *)
and checkMatchCasesForExtraArgumentAdd matchCasesList acc hashTableFunctionName =
  match matchCasesList with 
  | [] -> List.rev acc
  | {pc_lhs = pcLhs ; pc_guard = pcGuard;
    pc_rhs =  matchContent} :: rx -> 
      let newMatchCase = 
        {
          pc_lhs = pcLhs; 
          pc_guard = pcGuard;
          pc_rhs =  checkIfExpressionNeedsExtraArgument matchContent hashTableFunctionName
        } 
      in
      checkMatchCasesForExtraArgumentAdd rx (newMatchCase::acc) hashTableFunctionName
and evaluateValueBindingListForExtraArgumentAdd  hashTableFunctionName acc = function
  | [] -> List.rev acc
  | { pvb_pat = {ppat_desc = Ppat_var {txt = functionName; loc = loc};
      ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
      pvb_expr = expression; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
    } ::rx -> 
      (* Transformação para ANF *)
      let newPvbExp = checkIfExpressionNeedsExtraArgument expression hashTableFunctionName in
      let newValueBind =
      {
          pvb_pat = {ppat_desc = Ppat_var {txt = functionName; loc = loc};
          ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
          pvb_expr = newPvbExp; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
      } 
      in 
      evaluateValueBindingListForExtraArgumentAdd  hashTableFunctionName (newValueBind::acc) rx
  (* E.g Let () = ... *)
  | { pvb_pat = {ppat_desc = Ppat_construct ({txt = functionName; loc = loc}, option);
      ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
      pvb_expr = expression; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
    } ::rx -> 
      (* Transformação para ANF *)
      let newPvbExp = checkIfExpressionNeedsExtraArgument expression hashTableFunctionName in
      let newValueBind =
      {
          pvb_pat = {ppat_desc = Ppat_construct ({txt = functionName; loc = loc}, option);
          ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
          pvb_expr = newPvbExp; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
      }  
      in 
      evaluateValueBindingListForExtraArgumentAdd hashTableFunctionName (newValueBind::acc) rx
  (* E.g Let _ = ... *)
  | { pvb_pat = {ppat_desc = Ppat_any;
      ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
      pvb_expr = expression; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
    } ::rx -> 
      (* Transformação para ANF *)
      let newPvbExp = checkIfExpressionNeedsExtraArgument expression hashTableFunctionName in
      let newValueBind =
      {
          pvb_pat = {ppat_desc = Ppat_any;
          ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
          pvb_expr = newPvbExp; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
      }  
      in 
      evaluateValueBindingListForExtraArgumentAdd hashTableFunctionName (newValueBind::acc) rx
  (* Não promover transformação *)
  | exp::rx -> evaluateValueBindingListForExtraArgumentAdd hashTableFunctionName (exp::acc) rx  
 
(* --------------------------------------------------------------------------- *)

(* Avaliar structure itens *)
let rec eval_structure_item mapper item acc =
  match item with
  (* Transformar em ANF, transformação intermédia de CPS, e posteriormente em CPS *)
  | { pstr_desc = Pstr_extension (({ txt = "CPS"; _ }, pstr), _); _} -> 
  begin 
    match pstr with 
    (* Avaliar label e avaliaremos a sua expressão (pvb_expr) *)
    | (PStr [{ pstr_desc =
        Pstr_value (label,
          pvbList) 
      ; _}
      ]) -> 
      begin
        (* A origem de pvbList (ser lista e não só um elemento) surge quando aplicamos a tag 
        %CPS a um let rec f ... and y ... *)
        let rec avaliaPvbList acc = function
          | [] -> List.rev acc
          | exp :: rx ->
            (* Transformar cada exp de pvbList para CPS *)
            let newExp = transformExpToCPS label exp in
            avaliaPvbList (newExp::acc) rx
        in
        let cpsPvbList = avaliaPvbList [] pvbList in 
        (* Nova função, com pvbList em formato CPS *)
        let newFunction = 
          { pstr_desc =
            Pstr_value (label, 
              cpsPvbList); 
            pstr_loc = !(default_loc) 
          }
        in 
        default_mapper.structure_item mapper newFunction :: acc
      end
    (* Caso a tag esteja associada a outra instrução que a estrutura contemplada, 
    remover a tag para que o código possa ser compilado (a tag não produzira alterações no código) *)
    | PStr [otherInstruction] -> default_mapper.structure_item mapper otherInstruction :: acc    
    (* Caso tenha tido a tag deverá ser do tipo PStr[...]. Este assert false é para garantir que
    o código nunca deverá chegar a este ponto *)
    | _ -> assert false
  end
  (* Avaliar restantes expressões para verificar se teremos que adicionar argumento extar a algumas
  destas -> corresponde ao caso em que houve transformações CPS e foi adicionado o argumento kFunction *)
  | {pstr_desc = Pstr_value (label,valueBindingList); pstr_loc = loc }  ->
    begin
        let newValueBindingList = 
          evaluateValueBindingListForExtraArgumentAdd (!hashTableFunctionName) [] valueBindingList in
        let item = {pstr_desc = Pstr_value (label,newValueBindingList); pstr_loc = loc } in
        default_mapper.structure_item mapper item :: acc 
    end 
  | _ -> default_mapper.structure_item mapper item :: acc 

and transformExpToCPS label = function
  (* Estrutura para promover alteração para CPS *)
  | { pvb_pat = {
        ppat_desc = Ppat_var {txt = functionName; loc = loc};
        ppat_attributes = ppatAtt;
        ppat_loc = ppatLoc;
      }; 
      pvb_expr = pvbExpression; 
      pvb_attributes = pvbAtt; 
      pvb_loc = pvbLoc
    } ->
      (* Alteração de corpo da função para ANF *)
        let newBodyFunction = changeFunctionToANF pvbExpression functionName in
        (* Alteração de corpo para CPS, em casos de recursividade *)
        let newBodyFunctionCPS = 
          match label with
          | Recursive -> (* newBodyFunction *) changeFunctionToCPS newBodyFunction
          | Nonrecursive ->  
            (* Explicação na seção dedicada às variáveis (topo da página) *)
            let () = applyExtraArgumentFromCPSTransf := true in
            let newBodyCps = changeFunctionToCPS newBodyFunction in
            let () = applyExtraArgumentFromCPSTransf := false in
            newBodyCps
        in
        { pvb_pat = {
            ppat_desc = Ppat_var {txt = functionName; loc = loc};
            ppat_attributes = ppatAtt;
            ppat_loc = ppatLoc;
          }; 
          pvb_expr = newBodyFunctionCPS; 
          pvb_attributes = pvbAtt; 
          pvb_loc = pvbLoc
        }
  | exp -> exp
;;

let structure_mapper mapper structure  =
  List.fold_right (eval_structure_item mapper) structure []
;;

let ppx_mapper argv = 
  { 
    default_mapper with 
    structure = structure_mapper;
  }
 
let () = register "ppxCPS" ppx_mapper




 