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
let applyK = ref false

(* Variável para perceber se a inserção de função Aux interna (contendo mesmo argumentos +
kFuncition (fun s -> s)), resultante da aplicação CPS, para aplicar ao let ... mais externo 
(onde está a extensão CPS) ou nos let ... dentro deste. Caso a extensão seja aplicada a um let 
não recursivo, então todos os lets dentro deste terão a função Aux in aplicada.Caso a extensão 
seja aplicada a um let recursivo então a adição de funcao Aux in só será a este let e não aos 
internos a este *)
let applyFuncInAuxFromCPSTransfOnlyToExternalLetIn = ref false

(* Avaliar se ambiente é recursivo (estamos dentro de let rec ...) para promover a transformações
diferentes em ANF (caso particular de List) e personalizadas em CPS (criação imediata de funcAuxIn
ou apenas criar dentro de let rec internos) *)
let recEnvironment = ref false

(* Usanda para verificar se ambiente é um contrutor (E.g ::), na aplicação de kFunction, em 
particular quando usando ITE. Em ITE a variável de aplicação de kFunction (applyK) é resetada 
para verdade. Na grande maioria dos casos isso é correto, mas no caso particular de termos ITE
dentro de elementos de construtores (ou aplicações, embora não testado) esta não deve ser
resetada sob pena de termos aplicação kFuntion e elementos dentro de contructor e ao contrutor 
também (E.g. k (if c then k e1 else k e2) :: rx -> incorreto). De forma análoga se pode pensar
no caso de aplicações e tuplos *)
let constructApplyTuppleEnvironment = ref false

(* Auxiliar para perceber se teremos de retirar label "rec" de função let rec. Acontece quando 
criamos a funcaoAuxIn dentro desta (com argumento extra kFunction, (fun s -> s)). Neste caso não se 
justifica que a função mais externa matenha a tag recursiva visto apenas haver recursividade na
funcaoAuxIn *)
let removeRecursiveTag = ref false

(* Variável para detetar se a função tem uma exceção presente, e nesse caso, deveremos aplicar
a continuação da exceção (ao invés da continuação de retorno, a comum) e adicionar um argumento
extra à função (kException). Esta variável é avaliada a cada let (em pvbPatList), caso o let mais
externo (com tag CPS) não seja recursivo. Caso seja será avaliada logo à cabeça (em 
transformExpToCPS) *)
let functionNumberOfExceptions = ref 0

(* Variável que indica se o match case a analisar provem de match ou de try with *)
let matchCaseOfTryWith = ref false

(* Lista de exceções e outputs (de try ... with ou de dentro de funções) *)
let listExceptionAndReturn : (Asttypes.arg_label * Parsetree.expression) list ref = ref []
(* Identificador de exceções, usado em applyKException, ou seja, o identificador para garantir
que kException_id não tem id repetido *)
let idException = ref 0

(* Lista de exceções de utilizador, preenchida durante a primeira análise ao programa, que serve
para identificar quais as withCases (de try ... with) a eliminar, quando introduzido exceções por
continuação. Caso haja um caso, nos withCases, de uma exceção não criada pelo utilizador (exceção
provém de Hashtbl, Not_found, por exemplo) então try .. with não pode ser eliminado *)
let listExceptionFromUser : (string) list ref = ref []
(* Variável auxiliar para perceber se try ... with deve ser eliminado *)
let shouldTryWithDisappear = ref false

                                    (* Variáveis CPS *)

(* Expressão fun _ -> _ *) 
let fun_to_ fromExp toExp = 
  (Nolabel,{pexp_desc = 
    Pexp_fun (Nolabel, None,
      fromExp,
      toExp
    );
  pexp_attributes = [];
  pexp_loc = !(default_loc);
  })
;;

(* Nome de var em fun _ -> ... *)
let fun_ var =
  {ppat_desc = Ppat_var {txt = var; loc = !(default_loc)}; 
  ppat_attributes = []; ppat_loc = !(default_loc)}

(* Caso particular de fun .. -> ... : fun s -> s *)
let _toS var =
  {pexp_desc = Pexp_ident {txt = Lident var; loc = !(default_loc)}; 
  pexp_attributes = []; pexp_loc = !(default_loc;)}

(* Expressão (), usado para tratamento de exceções, na aplicação de kException *)
let openAndCloseBracketsKException att loc = 
  {
    pexp_desc = Pexp_construct ({txt = Lident "()"; loc = !(default_loc)}, None);
    pexp_attributes = att; pexp_loc = loc
  }

(* Expressão (), usado para tratamento de exceções, usado em fun () -> exp *)
let openAndCloseBracketsFun_ = 
  {
    ppat_desc = Ppat_construct ({txt = Lident "()"; loc = !(default_loc)}, None);
    ppat_attributes = []; ppat_loc = !(default_loc)
  }

(* Hashtable que contém as exceções, definidas em try .. with e os seus retornos/outputs *)
let hashTableException = ref (Hashtbl.create 123456)

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

(* Auxiliar a verificar se o let in presente é o que contém matchArg, ou se vem de um anterior a 
ele *)
let correctLetInToAddMatchArg = function
  | {pexp_desc = (Pexp_match (_)); _} -> true
  | _ -> false  

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
  (* Tratamento de casos de constructores (E.g. Some (...), :: ) *)  
  | {pexp_desc = Pexp_construct(ident, Some(exp));
      pexp_attributes = att; pexp_loc = loc
    } ->
      let newExp = changeExpToMatchLetInVarName exp listExpLetIn in
      {
        pexp_desc = Pexp_construct(ident, Some(newExp));
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
  (* Caso de :: (e.g) usado em listas (List) ou N(...) em árvores *)
  | {pexp_desc = Pexp_construct(_, Some(exp)); _ }
    ->  
      let newAcc = gatherListOfRecursiveCall exp functionName acc in
      newAcc
  (* Caso de r :: f a (e.g, tuple -> r, f a), usado em listas (List) *)
  | {pexp_desc = Pexp_tuple(tupleList); _ } 
    ->
      let rec avaliaApplyList acc = function
        | [] -> List.rev acc
        | (el)::rx ->
          let newAcc = gatherListOfRecursiveCall el functionName acc in
          avaliaApplyList newAcc rx
      in       
      avaliaApplyList acc tupleList
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
      pexp_attributes = att; pexp_loc = loc }  ->
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
          (* Registo do valor anterior *)
          let recEnvironmentBeforeLet = !recEnvironment in
          let () = recEnvironment :=
            match recLabel with
              | Recursive -> true
              | Nonrecursive -> false
          in
          (* Transformação para ANF *)
          let newpvbPattList = changePvbPatListANF functionName [] pvbPattList in
          (* Repor valor *)
          let () = recEnvironment := recEnvironmentBeforeLet in
          let newNextLetInExp = evaluateExpressionANF functionName nextLetInexp in 
          let returnExp = {pexp_desc = Pexp_let (recLabel,
              newpvbPattList, 
              newNextLetInExp); 
          pexp_attributes = pexpAtt; pexp_loc = pexpLoc
          } in
          returnExp
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
    aninha-los, ficando "exp" no final do ultimo let in. Casos como tuplos ou constructs (E.g. ::)
    têm o mesmo tratamento (avaliados em gatherListOfRecursiveCall) *)
    | _ as exp ->
      (* Caso particular de List. E.g. let %CPS mapi f l = mapi 0 f l, faz referência a uma funcao
      mapi imediatamente acima desta. Neste caso, de forma bastante particular, a aplicação ANF 
      (e consequente transf para CPS) não deve ocorrer. Se ocorresse teriamos a aplicação de kFunction
      (fun s->s) numa função sem o argumento kFunction (pois não é recursiva). Assim avaliamos se 
      estamos num ambiente recursivo antes de transformar para ANF, o que culminará numa transformação
      para CPS. Na grande maioria dos casos, a condição em if será verdadeira *)
      if (!recEnvironment) then
        (* Recolher lista de aplicações para transformar em let in *) 
        let letInList = gatherListOfRecursiveCall exp functionName [] in
        (* Aplicar transformações *)
        let newExp = applyOfLetInExpressions exp functionName [] (List.rev letInList) in
        newExp
      else 
        (* Caso particular de List *) 
        exp

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
      (* Verificar se é preciso adicionar o argumento match (transformação de function para match) *)
      let evaluateLetIn = correctLetInToAddMatchArg pvbExpAnf in
      let newPvbExpAnf = 
        (* Garantir que a aplicação de matchArg não é executado no let errado (let interno) *)
        (if (!changeFunctionToMatch && evaluateLetIn) 
          then
            (* Repor valor de variável *)  
            let () = changeFunctionToMatch := false in
            addMatchArgFunctionToMatchWithTransformation pvbExpAnf
          else 
            pvbExpAnf) in
      (* Repor argumentos *)
      let pvbExpAnfWithArguments = changeBodyFunctionIgnoringArguments expression newPvbExpAnf in
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
      (* Verificar se é preciso adicionar o argumento match (transformação de function para match) *)
      let evaluateLetIn = correctLetInToAddMatchArg pvbExpAnf in
      let newPvbExpAnf = 
        (* Garantir que a aplicação de matchArg não é executado no let errado (let interno) *)
        (if (!changeFunctionToMatch && evaluateLetIn) 
          then 
          (* Repor valor de variável *)  
            let () = changeFunctionToMatch := false in
            addMatchArgFunctionToMatchWithTransformation pvbExpAnf
          else 
            pvbExpAnf) in
      (* Repor argumentos *)
      let pvbExpAnfWithArguments = changeBodyFunctionIgnoringArguments expression newPvbExpAnf in
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
      (* Verificar se é preciso adicionar o argumento match (transformação de function para match) *)
      let evaluateLetIn = correctLetInToAddMatchArg pvbExpAnf in
      let newPvbExpAnf = 
        (* Garantir que a aplicação de matchArg não é executado no let errado (let interno) *)
        (if (!changeFunctionToMatch && evaluateLetIn) 
          then
          (* Repor valor de variável *)   
            let () = changeFunctionToMatch := false in 
            addMatchArgFunctionToMatchWithTransformation pvbExpAnf
          else 
          pvbExpAnf) in
      (* Repor argumentos *)
      let pvbExpAnfWithArguments = changeBodyFunctionIgnoringArguments expression newPvbExpAnf in
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
(* Lista de exceções *)
let listException = ["raise"; "invalid_arg"; "failwith"]
(* Avaliar se identificador é exceção *)
let identIsException ident = List.mem ident listException
  
(* Verifica se a aplicação contém indentificadores associados a levantamento de exceções *)
let applyHasExceptionInIt = function
  | {pexp_desc = Pexp_ident {txt = Lident ident; _};
     _} -> identIsException ident
  | _ -> false 
;;
  
(* Obter o nome da exceção *)
let getExceptionNamePpat = function
  | {ppat_desc = Ppat_construct ({txt = Lident exceptionName; _}, _); _} 
    -> exceptionName
  | _  -> assert false

(* Particular: lista só tem um elemento *)
let getExceptionNamePexp = function
  | [(_,{pexp_desc = Pexp_construct ({txt = Lident exceptionName; _ }, _); _ })]  
    -> exceptionName
  |[(_,{pexp_desc = Pexp_constant (Pconst_string (exceptionName,_)); _ })]
    -> exceptionName
  | _ -> assert false

(* Avaliar se no corpo da função, dentro de let, haverá uma invocação de exceção. Conjunto de 3
funções capacitadas para avaliar cada expressão dentro da função *)
let rec evaluateException = function
  (* ITE, if não é aplicado CPS *) 
  | {pexp_desc = Pexp_ifthenelse (_, thenExp, Some(elseExp)); _}
    -> 
      (* let ifException = evaluateException ifExp in *)
      let _ = evaluateException thenExp in
      let elseExpcetion = evaluateException elseExp in
      elseExpcetion
  (* IT *)
  | {pexp_desc = Pexp_ifthenelse (_, thenExp, None);_}  
    -> 
     (* let ifException = evaluateException ifExp in *)
      let thenException = evaluateException thenExp in
      thenException
  (* Match case aninhados *)
  | {pexp_desc = (Pexp_match (_, matchCasesList)); _} 
    ->
      let matchCaseException = evaluateExceptionMatchCase matchCasesList in
      matchCaseException
  (* Function *)
  | {pexp_desc = Pexp_function(listMatchCases); _}
    ->
      let listMatchCaseException = evaluateExceptionMatchCase listMatchCases in
      listMatchCaseException
  (* Avaliar sequência *)
  | {pexp_desc = (Pexp_sequence (exp1, exp2)); _}
    -> 
      let _ = evaluateException exp1 in
      let exp2Exception = evaluateException exp2 in
      exp2Exception
  (* Avaliar try ... with Not_found (e.g) *)
  | {pexp_desc = (Pexp_try (exp, withList)); _}
    ->
      let _ = evaluateException exp in
      let withListException = evaluateExceptionMatchCase withList in
      withListException
  (* Caso de :: (e.g), usado em listas (List) *)
  | {pexp_desc = Pexp_construct(_, Some(exp)); _}
     ->
      let expException = evaluateException exp in
      expException
  (* Caso de r :: f a (e.g, tuple -> r, f a), usado em listas (List) *)
  | {pexp_desc = Pexp_tuple(tupleList); _}
    ->
      let rec evaluateTupleList = function
        | [] -> ()
        | el::rx -> 
          let _ = evaluateException el in
          evaluateTupleList rx
      in   
      let tupleListException = evaluateTupleList tupleList in
      tupleListException
  (* Let in, genericamente tratado *)
  | {pexp_desc = Pexp_let (_, pvbPattList, nextLetInexp); _}
    ->
      let _ = evaluateExceptionPvbPatList pvbPattList in
      let nextLetInException = evaluateException nextLetInexp in
      nextLetInException
  | {pexp_desc = Pexp_apply (exp, applyList); _}
    ->  
      let rec evaluateApplyList = function
        | [] -> ()
        | (_, exp) :: rx ->  
        let _ = evaluateException exp in
        evaluateApplyList rx
      in 
      (* A grande diferença nesta função *)
      let expException = applyHasExceptionInIt exp in
      if expException then 
        let () = functionNumberOfExceptions := !functionNumberOfExceptions + 1 in 
        ()
      else 
      let applyListException = evaluateApplyList applyList in
      applyListException 
  (* E.g assert false *)
  | {pexp_desc = Pexp_assert{ 
      pexp_desc = Pexp_construct ({txt = Lident "false"; _}, _);
     _};_} -> 
     let () = functionNumberOfExceptions := !functionNumberOfExceptions + 1 in 
     ()
  (* Casos de ident, constant, construtor sem parâmetro (false, true,e.g) constraint e outras
  não consideradas, não contêm exceção *)               
  | _ -> ()
and evaluateExceptionMatchCase matchCasesList =
  match matchCasesList with 
  | [] -> ()
  | {pc_lhs = _ ; pc_guard = _;
    pc_rhs =  matchContent} :: rx -> 
      let _ = evaluateException matchContent in
      evaluateExceptionMatchCase rx 

(* Avalia pvbPatList: conteudo dentro de let in ou let and sucessivos *)
and evaluateExceptionPvbPatList = function
  | [] -> ()
  | { pvb_pat = {ppat_desc = Ppat_var {txt = _; loc = _};
      ppat_attributes = _; ppat_loc = _} ; 
      pvb_expr = expression; _  
    } ::rx ->  
        let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
        let _ = evaluateException pvbExpWithoutArguments in
        evaluateExceptionPvbPatList rx
  (* E.g Let () = ... *)  
  | { pvb_pat = {ppat_desc = Ppat_construct ({txt = Lident _; loc = _}, _);
      ppat_attributes = _; ppat_loc = _};     
      pvb_expr = expression; pvb_attributes = _; pvb_loc = _
    } ::rx -> 
        let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
        let _ = evaluateException pvbExpWithoutArguments in
        evaluateExceptionPvbPatList rx
  | { pvb_pat = {ppat_desc = Ppat_tuple (_);
      ppat_attributes = _; ppat_loc = _};  
      pvb_expr = expression; pvb_attributes = _; pvb_loc = _
    } ::rx -> 
        let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
        let _ = evaluateException pvbExpWithoutArguments in
        evaluateExceptionPvbPatList rx
  (* Não promover transformação *) 
  | _ -> ()     

(* Avaliar try with, para obter retorno de exceçoes *) 

let rec evaluateTryWith = function 
  (* ITE, if não é aplicado CPS *) 
  | {pexp_desc = Pexp_ifthenelse (_, thenExp, Some(elseExp)); _}
    -> 
      (* let ifException = evaluateTryWith ifExp in *)
      let () = evaluateTryWith thenExp in
      evaluateTryWith elseExp
  (* IT *)
  | {pexp_desc = Pexp_ifthenelse (_, thenExp, None);_} 
    -> 
     (* let ifException = evaluateTryWith ifExp in *)
      evaluateTryWith thenExp
  (* Match case aninhados *)
  | {pexp_desc = (Pexp_match (_, matchCasesList)); _} 
    ->
      evaluateTryWithMatchCase matchCasesList
  (* Function *)
  | {pexp_desc = Pexp_function(listMatchCases); _}
    ->
      evaluateTryWithMatchCase listMatchCases
  (* Avaliar sequência *)
  | {pexp_desc = (Pexp_sequence (exp1, exp2)); _}
    -> 
      let () = evaluateTryWith exp1 in
      evaluateTryWith exp2
  (* Avaliar try ... with Not_found (e.g) *)
  (* O core desta função *)
  | {pexp_desc = (Pexp_try (exp, withList)); _}
    ->
      let () = matchCaseOfTryWith := true in
      let () = evaluateTryWith exp in
      let () = evaluateTryWithMatchCase withList in
      matchCaseOfTryWith := false
  (* Caso de :: (e.g), usado em listas (List) *)
  | {pexp_desc = Pexp_construct(_, Some(exp)); _}
     ->
      evaluateTryWith exp
  (* Caso de r :: f a (e.g, tuple -> r, f a), usado em listas (List) *)
  | {pexp_desc = Pexp_tuple(tupleList); _}
    ->
      let rec evaluateTupleList = function
        | [] -> ()
        | el::rx -> 
          let () = evaluateTryWith el in
          evaluateTupleList rx
      in   
      evaluateTupleList tupleList
  (* Let in, genericamente tratado *)
  | {pexp_desc = Pexp_let (_, pvbPattList, nextLetInexp); _}
    ->
      let () = evaluateTryWithPvbPatList pvbPattList in
      evaluateTryWith nextLetInexp
  | {pexp_desc = Pexp_apply (exp, applyList); _}
    ->  
      let rec evaluateApplyList = function
        | [] -> ()
        | (_, exp) :: rx ->  
        let () = evaluateTryWith exp in
        evaluateApplyList rx
      in 
      (* A grande diferença nesta função *)
      (*let expException = applyHasExceptionInIt exp in
      if expException then true else *)
      let () = evaluateTryWith exp in
      evaluateApplyList applyList
  (* E.g assert false *)
  | {pexp_desc = Pexp_assert{ 
      pexp_desc = Pexp_construct ({txt = Lident _; _}, _);
     _};_} -> ()
  (* Casos de ident, constant, construtor sem parâmetro (false, true,e.g) constraint e outras
  não consideradas, não contêm exceção *)               
  | _ -> ()
and evaluateTryWithMatchCase matchCasesList =
  match matchCasesList with 
  | [] -> ()
  | {pc_lhs = pcLhs ; pc_guard = _;
    pc_rhs =  matchContent} :: rx -> 
      (* Match case provém de try ... with *)
      if !matchCaseOfTryWith then 
        let () = 
          let exceptionName = getExceptionNamePpat pcLhs in
          Hashtbl.add !hashTableException exceptionName matchContent;
        in 
      let () = evaluateTryWith matchContent in
      evaluateTryWithMatchCase rx 

(* Avalia pvbPatList: conteudo dentro de let in ou let and sucessivos *)
and evaluateTryWithPvbPatList = function
  | [] -> () 
  | { pvb_pat = {ppat_desc = Ppat_var {txt = _; loc = _};
      ppat_attributes = _; ppat_loc = _} ; 
      pvb_expr = expression; _  
    } ::rx ->  
        let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
        let () = evaluateTryWith pvbExpWithoutArguments in
        evaluateTryWithPvbPatList rx
  (* E.g Let () = ... *)  
  | { pvb_pat = {ppat_desc = Ppat_construct ({txt = Lident _; loc = _}, _);
      ppat_attributes = _; ppat_loc = _};     
      pvb_expr = expression; pvb_attributes = _; pvb_loc = _
    } ::rx -> 
        let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
        let () = evaluateTryWith pvbExpWithoutArguments in
        evaluateTryWithPvbPatList rx
  | { pvb_pat = {ppat_desc = Ppat_tuple (_);
      ppat_attributes = _; ppat_loc = _};  
      pvb_expr = expression; pvb_attributes = _; pvb_loc = _
    } ::rx -> 
        let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
        let () = evaluateTryWith pvbExpWithoutArguments in
        evaluateTryWithPvbPatList rx
  (* Não promover transformação *) 
  | _ -> ()     




            (* ------- Criação de funcao Aux in e suas auxiliares----------------- *)
(* Altera nome de função (fName para fName_Aux) *)
let changeFunctionNameToAux newfunctionName = function
  | {
      ppat_desc = Ppat_var {txt = _; loc = loc};
      ppat_attributes = ppatAtt;
      ppat_loc = ppatLoc;
    } ->
    {
      ppat_desc = Ppat_var {txt = newfunctionName; loc = loc};
      ppat_attributes = ppatAtt;
      ppat_loc = ppatLoc;
    }
  | _ -> assert false 
;; 

(* Auxiliar para tornar reunir todos os argumentos da função principal e a passar para funcaoAux in *)
let rec gatherArgsToInFunctionAuxAndAddFuncIdent acc = function
    (* Ainda é argumento *)
    | {pexp_desc = Pexp_fun 
        (argLabel,_,
        {ppat_desc = ppatDesc; _},
        expression);
      pexp_attributes = att;
      pexp_loc = pexpLoc;
      } -> 
      (* Para considerar argumentos com constraint (e.g: (exp : t) ) *) 
      let rec evaluatePpatDesc = function
        | Ppat_var {txt = argName; _} -> 
            Pexp_ident {txt = Lident argName; loc = !(default_loc)}
        (* E.g. (exp : t)*)
        | Ppat_constraint ({ppat_desc = ppatDesc; _}, _) -> evaluatePpatDesc ppatDesc
        (* (e1,e2) -> caso particular (de duplo) *)
        | Ppat_tuple([{ppat_desc = ppatDesc1; _}; {ppat_desc = ppatDesc2; _}]) ->
          Pexp_tuple[ 
            {pexp_desc = evaluatePpatDesc ppatDesc1;
            pexp_attributes = [];
            pexp_loc = !(default_loc)}; 
            {pexp_desc = evaluatePpatDesc ppatDesc2; 
            pexp_attributes = [];
            pexp_loc = !(default_loc)}]
        | _ -> assert false
      in
      let argsFunction = (argLabel, 
        {pexp_desc = evaluatePpatDesc ppatDesc;
         pexp_attributes = att; pexp_loc = pexpLoc;
        })
      in   
      gatherArgsToInFunctionAuxAndAddFuncIdent (argsFunction::acc) expression
    (* Não é argumento, é corpo da função. Podemos devolver a lista acc, 
    depois de adicionarmos fun s -> s *) 
    | {pexp_desc = _; _} -> 
    (* Devolução final (fun s -> s), terá sempre pois é a continuação de retorno *)
    let fromExp = fun_ "s" in
    let toExp = _toS "s" in
    let funStoS = fun_to_ fromExp toExp in
    let returnList = 
      (* Verificar se devemos adicionar kException, quantas e quais as expressões de retorno (
        oriundas de Hashtbl, contendo expressões de retorno de try .. with) *)
      let newAcc =  (funStoS)::acc in
      !listExceptionAndReturn @ newAcc in
    List.rev returnList 
;; 

let addKException bodyFunction iterator =
let kExceptionName = "kException"^(string_of_int iterator) in
{  
  (* Adição do argumento kFunction (fun s -> s) ao conjunto de argumentos da função *)
  pexp_desc =
    Pexp_fun (Nolabel, None, 
      {
        ppat_desc = Ppat_var {
          txt = kExceptionName; loc = !(default_loc)
        };
        ppat_attributes = [];
        ppat_loc = !(default_loc);
      },     
    (* Se houver exceção, nessecita de receber um argumento extra *)
    bodyFunction
  );
  pexp_attributes = [];
  pexp_loc = !(default_loc);
}  
 
(* Auxiliar para novo corpo na função de addArgumentoToFunction *)
let newBodyFunction bodyFunction = 
let iterator = 0 in 
  let rec insertKException n =
    if n = !functionNumberOfExceptions then bodyFunction
    else addKException (insertKException (n+1)) n
  in
  let newBody = insertKException iterator in
  newBody

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
      let newBodyFunctionExp = newBodyFunction bodyFunction in
      let returnExp = { 
        (* Adição do argumento kFunction (fun s -> s) ao conjunto de argumentos da função
        sempre necessário, quando esta função é invocada *)
        pexp_desc =
          Pexp_fun (Nolabel, None,
            {
              ppat_desc = Ppat_var {
                txt = "kFunction"; loc = !(default_loc)
              };
              ppat_attributes = [];
              ppat_loc = !(default_loc);
            },     
          (* Se houver exceção, nessecita de receber um argumento extra *)
          newBodyFunctionExp
        );
        pexp_attributes = [];
        pexp_loc = !(default_loc);
      } in 
      returnExp  
;;

(* Criação de função Aux in *)
let createFuncAuxIn newVarName pvbPat expression =
  (* Nome da nome função internat -> fName_Aux *)
  let newPvbPat = changeFunctionNameToAux newVarName pvbPat in
  (* Criação de função interna *)
  let returnFunAux = {pexp_desc = Pexp_let 
    (Recursive,
    (* Corpo da funcao (E.g. let rec height aux =  ...) *) 
    [
      {pvb_pat = newPvbPat;
      (* Adicionar o argumento extra (fun s -> s) *)
      pvb_expr = addArgumentToFunction expression;
      pvb_attributes = [];
      pvb_loc = !(default_loc)}
    ],
    (* Argumentos de funcao (... in tree (fun s -> s)) *)
    { pexp_desc = Pexp_apply 
      ({pexp_desc = Pexp_ident {txt = Lident newVarName; loc = !(default_loc)};
        pexp_attributes = [];
        pexp_loc = !(default_loc);
      }, (gatherArgsToInFunctionAuxAndAddFuncIdent [] expression)
    ); 
      pexp_attributes = [];
      pexp_loc = !(default_loc);
    });
  pexp_attributes = []; pexp_loc = !(default_loc);
  } in 
  returnFunAux
  
            (* -------------------------------------------------------------------- *)

 
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

(* Alterar aplicação (quando esta é o nome da função) para ter o estilo CPS *)
let changeFuncNameInLetInExp newFunctionName = function
  | {pexp_desc = Pexp_ident {txt = Lident _; loc = loc}; 
    pexp_attributes = pexpAtt; 
    pexp_loc = pexpLoc} 
    -> begin
      {pexp_desc = Pexp_ident {txt = Lident newFunctionName; loc = loc}; 
      pexp_attributes = pexpAtt; 
      pexp_loc = pexpLoc}
    end
  (* E.g. Ldot -> Hashtbl.find *)
  | _ as exp -> exp
;;  

(* Aplicação de kFunction, continuação de retorno *)
let applyKFunction expression = 
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
(* Aplicação de kException, continuação de exceção *)
let applyKException expression = 
  let kExceptionName = "kException"^(string_of_int !idException) in
  let returnExp = {pexp_desc = Pexp_apply (
    {pexp_desc = Pexp_ident {txt = Lident kExceptionName; loc = !(default_loc)}; 
    pexp_loc = !(default_loc); 
    pexp_attributes = []
    },
    (* A expressão que já existia, aparece aplicada a k *)
    [(Nolabel, expression)]
    );
  pexp_loc = !(default_loc); 
  pexp_attributes = [];
  } in
  let () = idException := !idException + 1 in
  returnExp
;;
 
(* kException *)
let kExceptionNameExp exceptionName = 
  (Nolabel, {
    pexp_desc = Pexp_ident {txt = Lident exceptionName; loc = !(default_loc)};
    pexp_attributes = []; pexp_loc = !(default_loc);
  })

(* Aplicar tantas kException quanto o número de exceções presentes na função *)
let listExceptionsApplied functionNumberOfExceptions = 
  let iterator = 0 in
    let rec insertKException n acc =
      if n = functionNumberOfExceptions then acc 
      else
        (* let () = idException := !idException - 1 in *)
        let kExceptionName = "kException"^(string_of_int n) in
        let kExceptionExp = kExceptionNameExp kExceptionName in
        insertKException (n+1) (kExceptionExp::acc)
    in
  let listExceptions = insertKException iterator [] in
  listExceptions 
;; 
(* Auxiliar para percorrer todos os argumentos da aplicaçao da chamada recursiva em let in, chegar
ao fim da lista e adicionar o fun (varName -> ...), chamando depois changeExpressionLetInsToCPSStyleAux *)
let rec restructPexpLetInToCPS acc varName restExpression newFunctionName = function
  | [] ->
    let expCPs = changeExpressionLetInsToCPSStyle newFunctionName restExpression in
    let applyFunExpCPS =(Nolabel, {
      pexp_desc = 
        Pexp_fun (Nolabel, None, varName, (expCPs));
      pexp_attributes = [];
      pexp_loc = !(default_loc)  
      })
    in
    (* Se let tiver uma exceção, teremos que adicionar um argumento extra (kException) *)
    let listLetIns =
      if !functionNumberOfExceptions > 0 then
        (* Tenho de passar o numero de exceções como argumento *)
        let listExceptions = listExceptionsApplied (!functionNumberOfExceptions) in
        let accWithFunStoS = applyFunExpCPS::acc in
        listExceptions @ (accWithFunStoS)
      else
        applyFunExpCPS::acc 
    in
    let listLetInsRev = List.rev (listLetIns) in
    listLetInsRev
  | (label, exp)::rx ->  restructPexpLetInToCPS ((label, exp)::acc)  varName restExpression newFunctionName rx

(* Converter todos os let in's  em .... (fun ... -> ), CPS style
  É garantido que, ao chegar a esta função, a expressão já está em ANF *)
and changeExpressionLetInsToCPSStyle newFunctionName = function
    (* ITE, if não é aplicado CPS *)
    | {pexp_desc = Pexp_ifthenelse (ifExp, thenExp, Some(elseExp)); 
        pexp_attributes = att; pexp_loc = loc}
        -> 
        (* Caso particular *)
        let () = applyK := false in
        let newIfExp = changeExpressionLetInsToCPSStyle newFunctionName ifExp in
        let () = applyK := 
          if !constructApplyTuppleEnvironment then false else !recEnvironment in
        let newThenExp = changeExpressionLetInsToCPSStyle newFunctionName thenExp in
        let () = applyK := 
          if !constructApplyTuppleEnvironment then false else !recEnvironment in
        let newElseExp = changeExpressionLetInsToCPSStyle newFunctionName elseExp in
        let () = applyK := false in
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
        let () = applyK := false in
        let newIfExp = changeExpressionLetInsToCPSStyle newFunctionName ifExp in
        let () = applyK := 
          if !constructApplyTuppleEnvironment then false else !recEnvironment in
        let newThenExp = changeExpressionLetInsToCPSStyle newFunctionName thenExp in
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
        let newMatchCaseList = changeMatchCasesCPS matchCasesList [] newFunctionName in
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
        let newMatchCaseList = changeMatchCasesCPS matchCasesList [] newFunctionName in 
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
        (* let newExp1 = changeExpressionLetInsToCPSStyle newFunctionName exp1 in *)
        let newExp2 = changeExpressionLetInsToCPSStyle newFunctionName exp2 in  
        {
          pexp_desc = (Pexp_sequence (
            exp1, (* Não avaliar *)
            newExp2)
          ); 
          pexp_attributes = att; 
          pexp_loc = loc
        }
    (* Avaliar try ... with Not_found (e.g) *)
    | {pexp_desc = (Pexp_try (exp, withList)); 
       pexp_attributes = att; pexp_loc = loc} 
       ->
        let () = matchCaseOfTryWith := true in
        let () = shouldTryWithDisappear := true in
        let newExp = changeExpressionLetInsToCPSStyle newFunctionName exp in
        let newWithList = changeMatchCasesCPS withList [] newFunctionName in 
        let () = matchCaseOfTryWith := false in
        if !shouldTryWithDisappear then
          let () = shouldTryWithDisappear := true in
          newExp
        else 
        let returnExp = {
          pexp_desc = (Pexp_try (
            newExp,
            (* withList tem mesma estrutura de mathCase *) 
            newWithList)
          ); 
          pexp_attributes = att; 
          pexp_loc = loc
        } in
        returnExp
    (* Caso de :: (e.g), usado em listas (List) *)
    | {pexp_desc = Pexp_construct(lident, Some(exp));
        pexp_attributes = pAtt; pexp_loc = pLoc 
      } ->
        (* Verificar se devemos aplicar kFunction (fun s -> s). Para tal usaremos uma temporaria
          interna. Para evitar que aplicações recursivas sejam todas aplicadas a kFunction,
          alteraremos a variavel global para false *)
        let inApplyK = !applyK in
        let () = applyK := false in 
        let constructApplyTuppleEnvironmentVarBeforeEntering = !constructApplyTuppleEnvironment in 
        let () = constructApplyTuppleEnvironment := true in 
        let newExp = changeExpressionLetInsToCPSStyle newFunctionName exp in
        let temp = {
          pexp_desc = Pexp_construct(lident, Some(newExp));
          pexp_attributes = pAtt; pexp_loc = pLoc 
        } in
        let returnExp = 
          (if (inApplyK)
          then applyKFunction temp  
          else temp) in
        (* Variável usada em ITE para promover correta aplicação de kFunction. Repor
        o valor anterior à entrada em apply *) 
        let () = constructApplyTuppleEnvironment := constructApplyTuppleEnvironmentVarBeforeEntering in 
        returnExp
    (* Caso de r :: f a (e.g, tuple -> r, f a), usado em listas (List) *)
    | {pexp_desc = Pexp_tuple(tupleList);
      pexp_attributes = pAtt; pexp_loc = pLoc 
      } ->
        let rec evaluateTupleList acc = function
          | [] -> List.rev acc
          | el::rx -> 
            let evaluateEl = changeExpressionLetInsToCPSStyle newFunctionName el in
            evaluateTupleList (evaluateEl::acc) rx
        in   
        (* Verificar se devemos aplicar kFunction (fun s -> s). Para tal usaremos uma temporaria
          interna. Para evitar que aplicações recursivas sejam todas aplicadas a kFunction,
          alteraremos a variavel global para false *)
        let inApplyK = !applyK in
        let () = applyK := false in   
        let constructApplyTuppleEnvironmentVarBeforeEntering = !constructApplyTuppleEnvironment in
        let () = constructApplyTuppleEnvironment := true in 
        let newTupleList = evaluateTupleList [] tupleList in
        let temp = {
          pexp_desc = Pexp_tuple(newTupleList);
          pexp_attributes = pAtt; pexp_loc = pLoc 
        } in
        let returnExp = 
          (if (inApplyK)
          then applyKFunction temp  
          else temp) in
        (* Variável usada em ITE para promover correta aplicação de kFunction. Repor
        o valor anterior à entrada em apply *) 
        let () = constructApplyTuppleEnvironment := constructApplyTuppleEnvironmentVarBeforeEntering in 
        returnExp
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
          let temp = (restructPexpLetInToCPS [] varNamePatt nextLetInexp newFunctionName list) in
          {pexp_desc = Pexp_apply  
            (changeFuncNameInLetInExp newFunctionName pexp,
            (* Reestrutura ANF para CPS *) 
            temp
            );  
          pexp_attributes = att; 
          pexp_loc = loc} 
        else
          (* Garantir que avaliações seguintes têm em conta o ambiente de let. Se for não
          recursivo a aplicação de kFuntion não se justifica *)
          let recEnvironmentBeforeLet = !recEnvironment in
          let applyKBeforeLet = !applyK in
          let () = applyK :=
            match recLabel with
              | Recursive -> let () = recEnvironment := true in true
              | Nonrecursive -> let () = recEnvironment := false in false
          in 
          (* Avaliar de forma genérica *)
          let newpvbPattList = changePvbPatListCPS newFunctionName [] rx  in
          let newPvbExp = changeExpressionLetInsToCPSStyle newFunctionName expression in
          (* Repor variável de recEnvironmet e applyK para nextExp de let in *)
          let () = recEnvironment := recEnvironmentBeforeLet in
          let () = applyK := applyKBeforeLet in
          let newNextLetInExp = changeExpressionLetInsToCPSStyle newFunctionName nextLetInexp in 

          (* Avaliar se a label deverá ser atualizada (no caso de ser incorporada funcAuxIn) *)
          let recLabel = 
            if !removeRecursiveTag 
              (* Se for necessário remover rec, faze-lo *)
              then 
                (* Reset de variável é necessário senão let ... and, podem ficar 
                com problemas => não conhecer a proxima funcao pois removemos o rec
                do let anterior. *)
                let () = removeRecursiveTag := false in
                Nonrecursive 
              else recLabel 
          in
          let returnExp = {pexp_desc = Pexp_let (recLabel,
            { pvb_pat = {ppat_desc = Ppat_var {txt = functionName; loc = l};
              ppat_attributes = ppaAtt; ppat_loc = ppatLoc} ; 
              pvb_expr = 
                newPvbExp; 
                pvb_attributes = pvbAtt; pvb_loc = pvbLoc
                } ::newpvbPattList,   
            newNextLetInExp); 
            pexp_attributes = pexpAtt; pexp_loc = pexpLoc 
          } in  

          returnExp
    (* Let in, genericamente tratado *)
    | {pexp_desc = Pexp_let (recLabel,
        pvbPattList,  
        nextLetInexp); 
        pexp_attributes = pexpAtt; pexp_loc = pexpLoc 
      } ->
        (* Possível introdução de funcAuxIn, dentro de let, será avaliado dentro de 
        changePvbPatListCPS *)
        let recEnvironmentBeforeLet = !recEnvironment in
        let applyKBeforeLet = !applyK in
        let () = applyK :=
          match recLabel with 
            | Recursive -> let () = recEnvironment := true in true
            | Nonrecursive -> let () = recEnvironment := false in false
        in
        let newpvbPattList = changePvbPatListCPS newFunctionName [] pvbPattList in
        (* Avaliar se a label deverá ser atualizada (no caso de ser incorporada funcAuxIn) *)
        let recLabel = 
          if !removeRecursiveTag 
            (* Se for necessário remover rec, faze-lo *)
            then 
              (* Reset de variável é necessário senão let ... and, podem ficar 
              com problemas => não conhecer a proxima funcao pois removemos o rec
              do let anterior. *)
              let () = removeRecursiveTag := false in
              Nonrecursive 
            else recLabel 
        in
        (* Repor variável de recEnvironmet e applyK para nextExp de let in *)
        let () = recEnvironment := recEnvironmentBeforeLet in
        let () = applyK := applyKBeforeLet in
        let newNextLetInExp = changeExpressionLetInsToCPSStyle newFunctionName nextLetInexp in

        let newExp = {
          pexp_desc = Pexp_let (recLabel,   
          newpvbPattList,  
          newNextLetInExp); 
          pexp_attributes = pexpAtt; pexp_loc = pexpLoc 
        } in
        let () = applyK := false in 
        newExp
    | {pexp_desc = Pexp_apply (exp, applyList); 
      pexp_attributes = att; pexp_loc = loc}
          ->  
          (* Verificar se devemos aplicar kFunction (fun s -> s). Para tal usaremos uma temporaria
          interna. Para evitar que aplicações recursivas sejam todas aplicadas a kFunction,
          alteraremos a variavel global para false *)
          let inApplyK = !applyK in
          let () = applyK := false in
          let constructApplyTuppleEnvironmentVarBeforeEntering = !constructApplyTuppleEnvironment in
          let () = constructApplyTuppleEnvironment := true in
          let rec evaluateApplyList acc = function
            | [] -> List.rev acc
            | (label, exp) :: rx -> 
              (* Avaliar recursivamente a expressão *)
              let evaluateExp = changeExpressionLetInsToCPSStyle newFunctionName exp in
              evaluateApplyList ((label,evaluateExp)::acc) rx
          in 
          let newApplyList = evaluateApplyList [] applyList in
          let newExp = changeExpressionLetInsToCPSStyle newFunctionName exp in 
          let applyExp = {
            pexp_desc = Pexp_apply (newExp, newApplyList); 
            pexp_attributes = att; pexp_loc = loc
          } in
          (* Garantir que a aplicação de kFunction ocorre quando a função deste tem o 
          argumento extra. "not" (explicação no topo da página) para evitar que quando aplico
          %CPS a uma função não recursiva, instruções antes das funções recursivas dentro desta
          não tenham kFunction aplicado  *)
          let () = applyK := false in
          let hasException = applyHasExceptionInIt exp in
          let returnExp = 
            (* Avaliar se aplicação é uma exceção (não devemos aplicar kFunction) *)
            (if (inApplyK && (not hasException))
              then applyKFunction applyExp  
            else 
              (* No caso de ser, aplicar kException, caso o ambiente seja recursivo *)
              if (hasException && !recEnvironment)
                then
                  (* Aplicação de continuação exceção. Apply será alterado para () caso
                  a exceção se encontre na Hashtbl, ou seja, o programador definiu qual o
                  valor de retorno para o caso de a sua função encontrar a exceção. Caso não
                  tenha sido definido o valor de retorno (num try .. with) então a aplicação,
                  com exceção, não será modificada para () *)
                  try  
                    let exceptionName = getExceptionNamePexp applyList in
                    let exceptionReturn = Hashtbl.find !hashTableException exceptionName in
                    let exceptionPatt = openAndCloseBracketsFun_ in
                    let fun_to_= fun_to_ exceptionPatt exceptionReturn in
                    let () = listExceptionAndReturn := [fun_to_] @ (!listExceptionAndReturn) in
                    applyKException (openAndCloseBracketsKException att loc)
                  with Not_found ->
                    (* Não vamos buscar informação a hashtable pois ela não existe (não está
                    definida a exceção em try .. with). Assim substituimos a aplicação por ()
                    e passamos-a para continuação de exceção, ou seja, fun () -> applyExp *)
                    let exceptionPatt = openAndCloseBracketsFun_ in
                    let exceptionReturn = applyExp in
                    let fun_to_= fun_to_ exceptionPatt exceptionReturn in
                    let () = listExceptionAndReturn := [fun_to_] @ (!listExceptionAndReturn) in
                    applyKException (openAndCloseBracketsKException att loc)
                (* Não aplicar continuação de exceção *)
                else applyExp) in 
          (* Variável usada em ITE para promover correta aplicação de kFunction. Repor
          o valor anterior à entrada em apply *) 
          let () = constructApplyTuppleEnvironment := constructApplyTuppleEnvironmentVarBeforeEntering in 
          returnExp
    (* Identificador, nome de variável *)
    | { pexp_desc = Pexp_ident (_); _
      } as identExp -> 
        (* Verificar se devemos aplicar kFunction (fun s -> s) *)
        let returnExp = 
          (if (!applyK ) 
          then applyKFunction identExp 
          else identExp) in
        returnExp
    (* Constante *)
    | { pexp_desc = Pexp_constant (_); _
      } as constExp -> 
        (* Verificar se devemos aplicar kFunction (fun s -> s) *)
        let returnExp = 
          (if (!applyK)
          then applyKFunction constExp 
          else constExp) in
        returnExp
    (* Construtor sem "parametros". E.g: false, true *)
    | { pexp_desc = Pexp_construct (_, None); _
      } as constructExp ->
        (* Verificar se devemos aplicar kFunction (fun s -> s) *)
        let returnExp = (if !applyK then applyKFunction constructExp else constructExp) in
        returnExp  
     (* E.g let t : int = ... *)
    | {pexp_desc = Pexp_constraint (exp, _); _} ->
        let newExp = changeExpressionLetInsToCPSStyle newFunctionName exp in
        (*{
          pexp_desc = Pexp_constraint (newExp, coreType); 
          pexp_attributes = att; pexp_loc = loc
        }*) newExp
    (* E.g assert false -> aplicação de kException *)
    | {pexp_desc = Pexp_assert{ 
        pexp_desc = Pexp_construct ({txt = Lident "false"; _}, _);
      _}; pexp_attributes = att; pexp_loc = loc } as assertExp
        -> 
          (* Tratamento similar à de aplicações com exceções. No caso de assert false este não
          poderá aparecer em withCases, logo não é necessário procurar na Hashtable *)
          if (!recEnvironment) 
            then 
              (* Não vamos buscar informação a hashtable pois ela não existe (não está
              definida a exceção em try .. with). Assim substituimos a aplicação por ()
              e passamos-a para continuação de exceção, ou seja, fun () -> applyExp *)
              let exceptionPatt = openAndCloseBracketsFun_ in
              let exceptionReturn = assertExp in
              let fun_to_= fun_to_ exceptionPatt exceptionReturn in
              let () = listExceptionAndReturn := [fun_to_] @ (!listExceptionAndReturn) in
              applyKException (openAndCloseBracketsKException att loc)
            else 
              assertExp
    (* Expressões não consideradas *)               
    | _ as expression -> 
      expression

(* Tratar de matchCases e withCases (caso de try ... with Not_found, e.g.) *)
and changeMatchCasesCPS matchCasesList acc newFunctionName =
  match matchCasesList with 
  | [] -> List.rev acc
  | {pc_lhs = pcLhs ; pc_guard = pcGuard;
    pc_rhs =  matchContent} :: rx -> 
      let () = applyK := !recEnvironment in
      (* Avaliar se try with deverá ser eliminado *)
      let () = if !matchCaseOfTryWith then
        (* Caso ja tenhamos a informação que não se deva eliminar try...with, ou seja, existe uma 
        exceção em withList que não foi declarada pelo utilizador, então não vale a pena avaliar *)
        if !shouldTryWithDisappear then 
          let exceptionName = getExceptionNamePpat pcLhs in
          (* Procura por exceção na lista de exceções do utilizador. Try with deve desaparecer
          se houver, pelo menos, uma exceção na withList que não tenha sido declarada pelo utilizador *)
          let () = shouldTryWithDisappear := (List.mem exceptionName !listExceptionFromUser) in   
          ()
      in 
      let newMatchCase = 
        {
          pc_lhs = pcLhs; 
          pc_guard = pcGuard;
          pc_rhs =  changeExpressionLetInsToCPSStyle newFunctionName matchContent
        } 
      in
      (* Retirar withCase, caso este venha de try .. with e seja referente a uma exceção do 
      utilizador. Neste caso a exceção será tratada dentro da função correspondente, com recurso
      a continuação de exceção (CPS) *)
      if (!matchCaseOfTryWith) then
        let exceptionName = getExceptionNamePpat pcLhs in
        let exceptionFromUser = List.mem exceptionName !listExceptionFromUser in
        if exceptionFromUser then
            changeMatchCasesCPS rx (acc) newFunctionName
          else
            changeMatchCasesCPS rx (newMatchCase::acc) newFunctionName
      else
        changeMatchCasesCPS rx (newMatchCase::acc) newFunctionName

(* Avalia pvbPatList: conteudo dentro de let in ou let and sucessivos *)
and changePvbPatListCPS newFunctionName acc = function
  | [] -> List.rev acc 
  | { pvb_pat = {ppat_desc = Ppat_var {txt = functionName; loc = loc}; 
      ppat_attributes = ppaAtt; ppat_loc = ppatLoc} as pvbPat; 
      pvb_expr = expression; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
    } ::rx -> 
    begin
      (* Valor da variável antes de entrar em let *) 
      let functionNumberOfExceptionsBeforeLet  = !functionNumberOfExceptions in
      (* Avaliaremos se devemos aplicar exceção *)
      let () = 
        (* Se valor anterior é false, avaliamos se existe exceção dentro de novo let *)
        if functionNumberOfExceptionsBeforeLet = 0 then 
          (* Avaliação de valor de variável *) 
            let temp = returnFunctionIgnoringArguments expression in
            let _ = evaluateException temp in ()
          (* Retem o mesmo valor *)
      in 
      let pvbExpCPSWithArguments =
        (* Avaliação genérica, aplicação de funcAuxIn não deverá ocorrer em let internos *)
        if (!applyFuncInAuxFromCPSTransfOnlyToExternalLetIn) then begin
          (* Retirar argumentos, para não interferir com avaliação *)
          let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
          (* Transformação para CPS *)
          let pvbExpCPS = changeExpressionLetInsToCPSStyle newFunctionName pvbExpWithoutArguments in
          (* Retirar argumentos, para não interferir com avaliação *)
          let pvbExpCPSWithArguments = changeBodyFunctionIgnoringArguments expression pvbExpCPS in
          pvbExpCPSWithArguments 
          end  
        else 
          (* Inserir funcAuxIn dentro do let, e dentro deste transformar para CPS. Registar
          informação que label rec deverá ser retirada do let (pois recursividade virá da nova
          funcaoAuxIn criada) Reposição do valor para false será feito no let (na funcao
          changeExpressionLetInsToCPSStyle) *)
          let () = removeRecursiveTag := false in 
          let expWithFuncAuxIn = changeFunctionToCPSWithFuncAuxIn expression functionName pvbPat in
          expWithFuncAuxIn
      in
      let tempCPS = 
      {
          pvb_pat = {ppat_desc = Ppat_var {txt = functionName; loc = loc};
          ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
          pvb_expr = pvbExpCPSWithArguments; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
      } 
      in
      (* Reposição de valor à entrada de let *)
      let () = functionNumberOfExceptions  := functionNumberOfExceptionsBeforeLet in 
      changePvbPatListCPS newFunctionName (tempCPS::acc) rx
  end
  (* E.g Let () = ... *)  
  | { pvb_pat = {ppat_desc = Ppat_construct ({txt = Lident varName; loc = loc}, option);
      ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
      pvb_expr = expression; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
    } ::rx -> 
      (* Valor da variável antes de entrar em let *) 
      let functionNumberOfExceptionsBeforeLet  = !functionNumberOfExceptions in
      (* Avaliaremos se devemos aplicar exceção *)
      let () = 
        (* Se valor anterior é false, avaliamos se existe exceção dentro de novo let *)
        if functionNumberOfExceptionsBeforeLet = 0 then 
          (* Avaliação de valor de variável *) 
            let temp = returnFunctionIgnoringArguments expression in
            let _ = evaluateException temp in ()
      in 
      (* Retirar argumentos, para não interferir com avaliação *)
      let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
      (* Transformação para CPS *)
      let pvbExpCPS = changeExpressionLetInsToCPSStyle newFunctionName pvbExpWithoutArguments in
      (* Retirar argumentos, para não interferir com avaliação *)
      let pvbExpCPSWithArguments = changeBodyFunctionIgnoringArguments expression pvbExpCPS in
      let tempCPS =
      {
          pvb_pat = {ppat_desc = Ppat_construct ({txt = Lident varName; loc = loc}, option);
          ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
          pvb_expr = pvbExpCPSWithArguments; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
      } 
      in 
      (* Reposição de valor à entrada de let *)
      let () = functionNumberOfExceptions  := functionNumberOfExceptionsBeforeLet in 
      changePvbPatListCPS newFunctionName (tempCPS::acc) rx
  | { pvb_pat = {ppat_desc = Ppat_tuple (tuppleList);
      ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
      pvb_expr = expression; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
    } ::rx -> 
      (* Valor da variável antes de entrar em let *) 
      let functionNumberOfExceptionsBeforeLet  = !functionNumberOfExceptions in
      (* Avaliaremos se devemos aplicar exceção *)
      let () = 
        (* Se valor anterior é false, avaliamos se existe exceção dentro de novo let *)
        if functionNumberOfExceptionsBeforeLet = 0 then 
          (* Avaliação de valor de variável *) 
            let temp = returnFunctionIgnoringArguments expression in
            let _ = evaluateException temp in ()
      in 
      (* Retirar argumentos, para não interferir com avaliação *)
      let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
      (* Transformação para CPS *)
      let pvbExpCPS = changeExpressionLetInsToCPSStyle newFunctionName pvbExpWithoutArguments in
      (* Retirar argumentos, para não interferir com avaliação *)
      let pvbExpCPSWithArguments = changeBodyFunctionIgnoringArguments expression pvbExpCPS in
      let tempCPS = 
      {
        pvb_pat = {ppat_desc =  Ppat_tuple (tuppleList);
        ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
        pvb_expr = pvbExpCPSWithArguments; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
      }   
      in
      (* Reposição de valor à entrada de let *)
      let () = functionNumberOfExceptions  := functionNumberOfExceptionsBeforeLet in 
      changePvbPatListCPS newFunctionName (tempCPS::acc) rx
  (* Não promover transformação *)
  | exp::rx -> changePvbPatListCPS newFunctionName (exp::acc) rx  

(* Transforma função em estilo CPS (assumindo que esta está em ANF), com funcao Aux in *)
and changeFunctionToCPSWithFuncAuxIn expression varName pvbPat =
  (* Resetar variável de aplicação de kFunction (fun s -> s) *)
  let () = applyK := true in
  let newVarName = varName^"_aux" in 
  (* Obter corpo da função e passar para CPS *)
  let newBody = returnFunctionIgnoringArguments expression in
  (* Transformar para CPS *)
  let newExpCPS = changeExpressionLetInsToCPSStyle newVarName newBody in
  (* Nova cpsExp, adicionado os argumentos, para facilitar a inserção de funcAuxIn dentro desta *)
  let newExpCPSWithArgs = changeBodyFunctionIgnoringArguments expression newExpCPS in
  (* Insercao de funcAuxIn dentro do corpo da função *)
  let newBodyWithFuncAuxIn = createFuncAuxIn newVarName pvbPat newExpCPSWithArgs in
  (* Criar nova função, em estilo CPS, comos seus argumentos re-incorporados *)
  let newFunctionCPS = changeBodyFunctionIgnoringArguments expression newBodyWithFuncAuxIn in
  (* Retomar variável para false, para evitar que fora do âmbito da função Aux in, seja 
  aplicado kFunction *)
  let () = applyK := false in
  newFunctionCPS

(* Transforma função em estilo CPS (assumindo que esta está em ANF), sem incorporar funcao Aux in.
Para quando CPS é aplicado a um let f ..., não recursivo *)
let changeFunctionToCPS expression newFunctionName =
  (* Como esta função é chamada de um let %CPS f, não recursivo, não devemos permitir a 
  aplicaçãoK, a não ser que entremos num let rec interno *)
   let () = applyK := false in
  (* Obter corpo da função e passar para CPS *)
  let newBody = returnFunctionIgnoringArguments expression in
  let newExpCPS = changeExpressionLetInsToCPSStyle newFunctionName newBody in
  (* Criar nova função, em estilo CPS, comos seus argumentos re-incorporados *)
  let newFunctionCPS = changeBodyFunctionIgnoringArguments expression newExpCPS in
  newFunctionCPS

(*************************************************************************************************)
(*                                                                                               *)
(*                                  Defuncionalization                                           *)
(*                                                                                               *)
(*************************************************************************************************)

(* Variável que indica se aplicação contém kFunction *)
let applyHasKFunction = ref false
(* Variável para auxilar a avaliação de fun's, derivados da transformação CPS, e construir os 
contrutores devidos *)
let funOfCps = ref false

(* Lista de construtores, resultantes da desfuncionalização dos fun de transformação CPS e 
variáveis destes, bem como retornos associados a construtores.
string -> nome de construtor
string lis -> variáveis livres de fun, correspondente a variáveis de construtor
Parsetree.expression -> expressão de retorno de construtor
 *)
let listConstructorAndItsReturn : (string * string list * Parsetree.expression ) list ref = ref []

(* Retornar nome de variável de pattern *)
let nameOfVarPattern = function
  |{ ppat_desc = Ppat_var {txt = varName; _}; _} -> varName
  | _ -> assert false

(* Expressão associada à criação de construtores resultantes de desfuncionalização => let varName =
arg in pexp_desc *)
let leTInExpressionOfDefuncConstructores varPatt pexp_desc =
{pexp_desc =
  Pexp_let (Nonrecursive,
    [({pvb_pat = varPatt;
      pvb_expr =
        {pexp_desc = Pexp_ident {txt = Lident "arg"; loc = !(default_loc)};
         pexp_attributes = []; pexp_loc = !(default_loc)};
      pvb_attributes = []; pvb_loc = !(default_loc)
    })],
    pexp_desc
    );
  pexp_attributes = [];
  pexp_loc = !(default_loc)
}

(* Variável*)
let bEvaluatinKFunction = ref false
(* Identificador dos contrutores, aquando da desfuncionalização *)
let idOfConstructorsDefunc = ref 0

(* Função que indica se aplicação contém kFunction. Para tal avaliamos o identificador associado à
aplicação *)
let checkIfApplyHasKFunction identOfApply =
  (* Avalia se identOfApply contém "kFunction", de Str *)
  let regExpAnf = regexp_string "kFunction" in 
  (* Procurar em identOfApply "kFunction" a partir da posição 0 e guarda resultado em variável
  referência *)
  let () = applyHasKFunction :=
    try  
      let _ = search_forward regExpAnf identOfApply 0 in true
    with 
      Not_found -> false 
  in ()
;;
 
(* Função que indica se identificador contém "_aux". Reaproveitamento para função anterior (avalia
nomes de funções) e para avaliar nomes de identificadores *)
let identHasPAtternInIt ident pattern =
  (* Avalia se ident contém "_aux", de Str *)
  let regExpAnf = regexp_string pattern in
  (* Procurar em ident "_aux" a partir da posição 0 *)
  try 
    let _ = search_forward regExpAnf ident 0 in true
  with 
    Not_found -> false 
;;

(* Função que avalia o nome da função, verificando se tem "_aux" nesta e retorna, um booleano e 
o nome da função sem aux (sob a forma de tuplo). Booleano indica se comtém aux ou não na função *)
let checkIfVarNamePattContainsPattern varNamePatt pattern =
  match varNamePatt with
  | {ppat_desc = Ppat_var {txt = varName; _}; _} -> identHasPAtternInIt varName pattern
  | _ ->  false 
;; 
 
(* Funcção que avalia nome de variáveis, de aplicações, para verificar quais as variáveis que
serão associadas a construtor (variáveis livres de fun). Estas variáveis não podem ser o nome da 
função, kFunction, nem o nome da variável de fun *)
let checkIfVarNamePexpContainsPattern varNamePexp pattern =
  match varNamePexp with
  | {pexp_desc = Pexp_ident {txt = Lident varName; _}; _} -> 
  let varnameHasPatt = identHasPAtternInIt varName pattern in
  (* Se tiver o padrão devolver o nome de variável e true, em tuplo. 
  Caso contrário, devolver nome de variável com false *)
  if (varnameHasPatt) then (varName, true) else (varName, false) 
  (* Caso não seja identificador, a alternativa é ser aplicação (e.g (n-1)), logo será
  variável a ser considerada, ou seja, não tem o padrão (pattern) que o excluiria *)

  (* Constantes não são variáveis de construtor, logo dizemos que contém padrão para que não
  seja incorporada no construtor *)
  | {pexp_desc = Pexp_constant (_); _} -> ("f", true)

  (* Outros casos, nomeadamente aplicações fora do âmbito de kFunction *)
  | _ ->  ("a",false) 
;;  

(* Função responsável por alterar nome de funções com "_aux" (funções internas criadas aquando da
transformação CPS) para "_def" (novo nome de função desfuncionalizada) *)
let changeFunctionNameToDef newfunctionName = function
  | {
      ppat_desc = Ppat_var {txt = _; loc = loc};
      ppat_attributes = ppatAtt;
      ppat_loc = ppatLoc;
    } ->
    {
      ppat_desc = Ppat_var {txt = newfunctionName; loc = loc};
      ppat_attributes = ppatAtt;
      ppat_loc = ppatLoc;
    }
  | _ -> assert false 
;;

(* Função responsável por aplicar "apply" a aplicações, caso estas contenham kFunction *)
let applyApply listExpression = 
  {pexp_desc = Pexp_apply (
    {pexp_desc = Pexp_ident {txt = Lident "apply"; loc = !(default_loc)}; 
    pexp_loc = !(default_loc); 
    pexp_attributes = []
    },
    (* A(s) expressão(ões) que já existia(m), aparece(m) aplicada(s) a apply *)
    listExpression
    );
  pexp_loc = !(default_loc); 
  pexp_attributes = [];
  }
;;

(* Função auxiliar à construção de contrutores para desfuncionalização *)
let constructDefuncPpat ident listVars returnConstruct = 

(* Criar lista de variáveis associadas ao construtor, com a terminologia correta  *)
let listPattOfVars = 
  List.fold_left 
    (* Para cada elemento de lista de variáveis, criar um pattern *)
    (fun acc el -> 
      ({ppat_desc = Ppat_var {txt = el; loc = !(default_loc)};
      ppat_attributes = []; ppat_loc = !(default_loc)}) :: acc)
    (* Elemento inicial *)
    [({ppat_desc = Ppat_var {txt = "kFunction"; loc = !(default_loc)};
    ppat_attributes = []; ppat_loc = !(default_loc)})]
    (* Lista de variáveis *)
    listVars
in
let returnExp =
{pc_lhs =
  {ppat_desc = 
    Ppat_construct ({txt = Lident ident; loc = !(default_loc)},
      Some
      {ppat_desc =
        Ppat_tuple listPattOfVars;
      ppat_attributes = [];
      ppat_loc = !(default_loc)
      }
    );
    ppat_attributes = [];
    ppat_loc = !(default_loc)
  };
  pc_guard = None;
  pc_rhs = returnConstruct
}
in returnExp

(* Função que construirá construtor que substituirá a primeira aparição de fun. Esta função serve
para adpatar a informação em matchCase de função apply para uma expressão, ou seja, em vez de termos
Ppat na construção de construtores, teremos Pexp. Muito similar à anterior *)
let constructDefuncPexp ident listVars = 
let listPexpOfVars = 
  List.fold_left 
    (* Para cada elemento de lista de variáveis, criar um pattern *)
    (fun acc el -> 
      ({pexp_desc = Pexp_ident {txt = Lident el; loc = !(default_loc)};
      pexp_attributes = []; pexp_loc = !(default_loc)}) :: acc)
    (* Elemento inicial *)
    [({pexp_desc = Pexp_ident {txt = Lident "kFunction"; loc = !(default_loc)};
    pexp_attributes = []; pexp_loc = !(default_loc)})]
    (* Lista de variáveis *)
    listVars
in
let returnExp = 
  Pexp_construct ({txt = Lident ident; loc = !(default_loc)},
    Some
    {pexp_desc =
      Pexp_tuple listPexpOfVars;
    pexp_attributes = [];
    pexp_loc = !(default_loc)
    }
  );
in returnExp

(* Função auxiliar para retornar pexp_desc de aplicações. Útil para devolver a applyApply e assim
ter um encadeamento de aplicações: apply kFunction expression, ao invés de 
apply (kFunction expression) *)
let restructPexpDescOfApply = function
  | {pexp_desc = Pexp_apply (exp, newApplyList); _} -> (Nolabel,exp) :: newApplyList
  | _ -> assert false 
;;
 
(* Função que remove os fun's, resultantes da transformação CPS, e o substitui pelo construtor
indicado, visando a desfuncionalização *)
let pexpDescOfApplyWithFunSubstitutedByConstruct expression constructor = 
  match expression with
  | {pexp_desc = Pexp_apply (exp, newApplyList); pexp_attributes = att; pexp_loc = loc} -> 
    (* Ao primeiro encontro de fun, substituir pelo construtor indicado, recebido como 
    parâmetro de função *)
    let rec removeFunsFromApplyList acc = function
      | [] -> List.rev acc
      | (_,el) as exp :: rx ->
        match el with
        (* Encontrado o local para substituir fun pelo construtor *)
        | {pexp_desc = Pexp_fun (_); _ } -> 
          let newAcc = (Nolabel, 
            {pexp_desc = constructor; 
            pexp_attributes = []; pexp_loc = !(default_loc)}
            )::acc in
          List.rev newAcc
        | _ -> removeFunsFromApplyList (exp::acc) rx
    in 
    let applyListWithoutFuns = removeFunsFromApplyList [] newApplyList in
    let returnExp = 
      {pexp_desc = Pexp_apply (exp, applyListWithoutFuns); pexp_attributes = att; pexp_loc = loc}
    in returnExp
  | _ -> assert false 
;; 

(* Função para encontrar variáveis para construtor, as variáveis livres de fun. O que vamos
avaliar será maioritariamente aplicações. Assim que encontrarmos fun, significa que podemos parar
a nossa pesquisa (será convertido noutro construtor este novo fun, logo não será tratado aqui).
Variáveis livres não poderão ser igual a nome de função nem a nome de variável de fun (fun var ->
exp), nem a kFunction (aplicação de continuação). O que acontece é que da maneira como a aplicação
funciona é que teremos uma exp em Pexp_apply, que é o primeiro elemento a aplicar aos restantes e
posteriormente uma lista de exp (em applyList). Acontece que o nome da função e kFuntion aparecem
sempre em exp, e a avaliação de variáveis apenas ocorrem em applyList, logo não precisamos de avaliar
se exp em applyList é igual a kFunction ou nome de função; apenas precisamos de garantir que esta
não é igual a nome de variável de fun (E.g fun (var -> ...)) *)
let rec varsOfConstrutor expression varOfFun =
  match expression with 
  | {pexp_desc = Pexp_apply (exp, applyList); _}
    -> 
    (* eprintf "Inicio : %s@." (Pprintast.string_of_expression exp); *)
    let beforeInbEvaluatinKFunction = !bEvaluatinKFunction in

    let rec getVarsOfContructor acc = function
      | [] -> List.rev acc
      | (_,el) :: rx -> 
        match el with
        | {pexp_desc = Pexp_fun (_); _ } -> 
          (* fun será convertido para construtor, em tempo apropriado. Aqui paramos a análise *)
          List.rev acc
        (* Justificação para esta diferença: 
        E.g. (fun fibANF_0  ->
           fib_aux (n - 1) (fun fibANF_1  -> kFunction (fibANF_1 + fibANF_0)
        Neste caso em (n - 1), quero avaliar como um todo, mas em (fibANF_1 + fibANF_0), quero
        avaliar o conteúdo da aplicação (+) presente *)
        | {pexp_desc = Pexp_apply (_); _} as applyExp ->
          (* Se a exp inicial é kFunction, analisar recursivamente aplicação *)
          let (_,expIsKFunction) = checkIfVarNamePexpContainsPattern exp "kFunction" in
          if (expIsKFunction || !bEvaluatinKFunction) then
            let () = bEvaluatinKFunction := true in
            let acc_temp = varsOfConstrutor applyExp varOfFun in
            getVarsOfContructor (acc_temp@acc) rx
          (* Caso contrário, fazer uma análise como se fosse qql outra expessão *)
          else 
            (* let () = eprintf "%b. SOS: %s@." !bEvaluatinKFunction (Pprintast.string_of_expression applyExp) in *)
            let (varName,bVarFunName) = checkIfVarNamePexpContainsPattern applyExp varOfFun in
            (* eprintf "%b. bVarFunNameSOS: %s@." bVarFunName (varName); *)
            if (not bVarFunName) then
              getVarsOfContructor (varName::acc) rx
            else  
              getVarsOfContructor (acc) rx
        | _ as exp ->   
          (* eprintf "EXPRESSION: %s@." (Pprintast.string_of_expression exp); *)
          (* Avaliar se condiz com nome de variável de fun *)
          let (varName,bVarFunName) = checkIfVarNamePexpContainsPattern exp varOfFun in
          (* eprintf "%b. bVarFunName_EXP: %s@." bVarFunName (varName); *)
          if (not bVarFunName) then
            getVarsOfContructor (varName::acc) rx
          else  
            getVarsOfContructor (acc) rx
    in  
    let listVarsConstrutors = getVarsOfContructor [] applyList in
    let () = bEvaluatinKFunction := beforeInbEvaluatinKFunction in
    listVarsConstrutors
  | _ -> assert false  
;;

(* Nome de construtor, recebendo identificador para distinção entre os diferentes construtores 
criados *) 
let constructorName id = "KConstructor_"^(string_of_int id)

(* Função inicial de apply, com Kid (continuação identidade), assumindo que será incorporado
em and com função aux, resultante da transformação CPS. Receberá uma lista de matches, contendo
todos os tipos kont's associados e respetivos retornos (desfuncionalization) *)
let applyFunction listConstrutors = 
(* Construtor inicial com Kid + listConstrutors *)
let listContructors = 
  {pc_lhs =
    {ppat_desc = 
      Ppat_construct ({txt = Lident "Kid"; loc = !(default_loc)}, None);
      ppat_attributes = []; ppat_loc = !(default_loc)};
    pc_guard = None;
    pc_rhs =
      {pexp_desc = Pexp_ident {txt = Lident "arg"; loc = !(default_loc)};
      pexp_attributes = []; pexp_loc = !(default_loc)}
  } :: listConstrutors in

let returnApplyFunction =
  {pvb_pat = 
    {ppat_desc = Ppat_var {txt = "apply"; loc = !(default_loc)}; 
      ppat_attributes = []; ppat_loc = !(default_loc)};
      pvb_expr =
      {pexp_desc =
        Pexp_fun (Nolabel, None,
          {ppat_desc = Ppat_var {txt = "kFunction"; loc = !(default_loc)}; 
          ppat_attributes = []; ppat_loc = !(default_loc)},
          {pexp_desc =
            Pexp_fun (Nolabel, None,
              {ppat_desc = Ppat_var {txt = "arg"; loc = !(default_loc)}; 
              ppat_attributes = []; ppat_loc = !(default_loc)},
              {pexp_desc =
                Pexp_match(
                  {pexp_desc = Pexp_ident {txt = Lident "kFunction"; loc = !(default_loc)};
                  pexp_attributes = []; pexp_loc = !(default_loc)},
                  listContructors
                );
                pexp_attributes = []; pexp_loc = !(default_loc)});
          pexp_attributes = []; pexp_loc = !(default_loc)});
      pexp_attributes = []; pexp_loc = !(default_loc)};
  pvb_attributes = []; pvb_loc = !(default_loc)} 
in
returnApplyFunction
;;

let rec changeExpressionToDefuncionalization newFunctionName = function
  (* ITE, if não é aplicado CPS *)
  | {pexp_desc = Pexp_ifthenelse (ifExp, thenExp, Some(elseExp)); 
      pexp_attributes = att; pexp_loc = loc}
      -> 
      let newIfExp = changeExpressionToDefuncionalization newFunctionName ifExp in
      let newThenExp = changeExpressionToDefuncionalization newFunctionName thenExp in
      let newElseExp = changeExpressionToDefuncionalization newFunctionName elseExp in
      let returnExp = {
        pexp_desc = Pexp_ifthenelse (
          newIfExp, 
          newThenExp, 
          Some(newElseExp)
          );  
          pexp_attributes = att; pexp_loc = loc
      } in
      returnExp
  (* IT *)
  | {pexp_desc = Pexp_ifthenelse (ifExp, thenExp, None); 
      pexp_attributes = att; pexp_loc = loc} 
      -> 
      let newIfExp = changeExpressionToDefuncionalization newFunctionName ifExp in
      let newThenExp = changeExpressionToDefuncionalization newFunctionName thenExp in
      let returnExp = { 
        pexp_desc = Pexp_ifthenelse (
          newIfExp, 
          newThenExp, 
          None
          ); 
          pexp_attributes = att; pexp_loc = loc
      } in
      returnExp
  (* function = ... *)
  | {pexp_desc = (Pexp_function (matchCasesList));
      pexp_attributes = att; 
      pexp_loc = loc
    } -> 
      let newMatchCaseList = changeMatchCasesDefuncionalization matchCasesList [] newFunctionName in
      let returnExp = {
        pexp_desc = (Pexp_function (
          newMatchCaseList));
        pexp_attributes = att; 
        pexp_loc = loc
      } in
      returnExp
  (* Match case aninhados *)
  | {pexp_desc = (Pexp_match (matchArg, matchCasesList));
      pexp_attributes = att; 
      pexp_loc = loc
    } ->
      let newMatchCaseList = changeMatchCasesDefuncionalization matchCasesList [] newFunctionName in 
      let returnExp = {
        pexp_desc = (Pexp_match 
          (matchArg,  
          newMatchCaseList));
        pexp_attributes = att; 
        pexp_loc = loc
      } in
      returnExp
  (* Avaliar sequência *)
  | {pexp_desc = (Pexp_sequence (exp1, exp2)); 
      pexp_attributes = att; pexp_loc = loc}
      -> 
      (* let newExp1 = changeExpressionToDefuncionalization newFunctionName exp1 in *)
      let newExp2 = changeExpressionToDefuncionalization newFunctionName exp2 in  
      let returnExp = {
        pexp_desc = (Pexp_sequence (
          exp1, (* Não avaliar *)
          newExp2)
        ); 
        pexp_attributes = att; 
        pexp_loc = loc
      } in
      returnExp
  (* Avaliar try ... with Not_found (e.g) *)
  | {pexp_desc = (Pexp_try (exp, withList)); 
      pexp_attributes = att; pexp_loc = loc} 
      ->
      let newExp = changeExpressionToDefuncionalization newFunctionName exp in
      let newWithList = changeMatchCasesDefuncionalization withList [] newFunctionName in 
      let returnExp = {
        pexp_desc = (Pexp_try (
          newExp,
          (* withList tem mesma estrutura de mathCase *) 
          newWithList)
        ); 
        pexp_attributes = att; 
        pexp_loc = loc
      } in
      returnExp
  (* Caso de :: (e.g), usado em listas (List) *)
  | {pexp_desc = Pexp_construct(lident, Some(exp));
      pexp_attributes = pAtt; pexp_loc = pLoc 
    } ->
      let newExp = changeExpressionToDefuncionalization newFunctionName exp in
      let returnExp = {
        pexp_desc = Pexp_construct(lident, Some(newExp));
        pexp_attributes = pAtt; pexp_loc = pLoc 
      } in
      returnExp
  (* Caso de r :: f a (e.g, tuple -> r, f a), usado em listas (List) *)
  | {pexp_desc = Pexp_tuple(tupleList);
    pexp_attributes = pAtt; pexp_loc = pLoc 
    } ->
      let rec evaluateTupleList acc = function
        | [] -> List.rev acc
        | el::rx -> 
          let evaluateEl = changeExpressionToDefuncionalization newFunctionName el in
          evaluateTupleList (evaluateEl::acc) rx
      in   
      let newTupleList = evaluateTupleList [] tupleList in
      let returnExp = {
        pexp_desc = Pexp_tuple(newTupleList);
        pexp_attributes = pAtt; pexp_loc = pLoc 
      } in
      returnExp
  (* Let in, genericamente tratado *)
  | {pexp_desc = Pexp_let (recLabel,
      pvbPattList,  
      nextLetInexp); 
      pexp_attributes = pexpAtt; pexp_loc = pexpLoc 
    } ->
      let newpvbPattList = changePvbPatListDefuncionalization newFunctionName [] pvbPattList in
      let newNextLetInExp = changeExpressionToDefuncionalization newFunctionName nextLetInexp in
      let returnExp = {
        pexp_desc = Pexp_let (recLabel,   
        newpvbPattList,  
        newNextLetInExp); 
        pexp_attributes = pexpAtt; pexp_loc = pexpLoc 
      } in
      returnExp
  | {pexp_desc = Pexp_apply (exp, applyList); pexp_attributes = att; pexp_loc = loc}
      ->  
      let rec evaluateApplyList acc = function
        | [] -> List.rev acc
        | (label, exp) :: rx -> 
          (* Avaliar recursivamente a expressão *)
          let evaluateExp = changeExpressionToDefuncionalization newFunctionName exp in
          evaluateApplyList ((label,evaluateExp)::acc) rx
      in 
      let newApplyList = evaluateApplyList [] applyList in
      let newExp = changeExpressionToDefuncionalization newFunctionName exp in
      (* Aquando da avaliação de expressão, a variável correspondente a verificação se aplicação 
      contém "kFunction" será alterada, caso tal situação se verifique. Caso contenha, deverá
      ser aplicado a esta aplicação "apply", visando a desfuncionalização. Apply recebe dois argumentos
      daí as modificações a efetuar serem as listadas abaixo *)
      let applyIdent =
        if !applyHasKFunction then
          {pexp_desc = Pexp_ident {txt = Lident "apply"; loc = !(default_loc)}; 
            pexp_loc = !(default_loc); 
            pexp_attributes = []
          }
        else
          newExp
      in
      let applyContent =
        if !applyHasKFunction then
          (* apply kunction exp *)
          (Nolabel,newExp) :: newApplyList
        else
          newApplyList
      in

      let returnExp = {
        pexp_desc = Pexp_apply (applyIdent, applyContent); 
        pexp_attributes = att; pexp_loc = loc
      } in
      returnExp 
  (* Identificador, nome de variável. Promover alteração caso contenha "_aux" para "_def" e 
  indicar aplicação associada que contem kFunction (aplicação de retorno em CPS), caso seja o caso
  usando uma variável referência *)
  | { pexp_desc = Pexp_ident {txt = Lident varName; loc = loc};
      pexp_attributes = pexpAtt; pexp_loc = pexpLoc } ->
      (* Avaliar se identificador tem kFunction, promovendo alteração (dentro da função 
      checkIfApplyHasKFunction) de variável referência associada *)
      let () = checkIfApplyHasKFunction varName in
      (* Avaliar nome de identificador e modificar este caso contenha "_aux" *)

      (* let checkIdentForAux = identHasPAtternInIt varName "_aux" in
      let varName = 
        if checkIdentForAux then varName^"_def" else varName in *)

      let returnExp = {
        pexp_desc = Pexp_ident {txt = Lident varName; loc = loc};  
        pexp_attributes = pexpAtt; pexp_loc = pexpLoc 
      } in
      returnExp 
  (* Constante *)
  | { pexp_desc = Pexp_constant (_); _} as constExp -> constExp
  (* Construtor sem "parametros". E.g: false, true *)
  | { pexp_desc = Pexp_construct (_, None); _} as constructExp -> constructExp
    (* E.g let t : int = ... *)
  | {pexp_desc = Pexp_constraint (exp, _); _} ->
      let returnExp = changeExpressionToDefuncionalization newFunctionName exp in
      returnExp
  (* E.g assert false -> aplicação de kException *)
  | {pexp_desc = Pexp_assert{  
      pexp_desc = Pexp_construct ({txt = Lident "false"; _}, _);
    _}; _ } as assertExp -> assertExp 
  (* Core da transformação para desfuncionalização: fun *)
  | {pexp_desc = Pexp_fun (argLabel,expOption,pattern,expression); 
     pexp_attributes = att; pexp_loc = loc} -> 
      (* Guardar valor de variável, antes de ser avaliada a expressão *)
      let beforeInFunOfCps = !funOfCps in  
      let () = funOfCps := true in

      let newExpression = changeExpressionToDefuncionalization newFunctionName expression in
      (* Reaproveitamento de função usada em ANF, para verificar se variável associada a fun
      (fun var -> ...), contém "ANF_" *)
      let pattHasANF_ = checkIfApplicationComesFromANFTransformation pattern in
      (* Verificar se função é do tipo (fun s -> s), usada na transformação CPS e subtituir por
      construtor de desfuncionalização *)
      let pattHas_S = checkIfVarNamePattContainsPattern pattern "s" in

      (* Caso pattHas_S = true e pattHasANF_ = true, então devemos colocar pattHas_S a false para
      que o returnPexpDesc seja correto. Esta situação ocorre quando fun tem um avariável com ANF e
      também contém um s (e.g fun (sumANF -> ...)) *)
      let pattHas_S = if pattHasANF_ then false else pattHas_S in

      let returnPexpDesc =
        if pattHas_S
          (* Substituir (fun s -> s) por contrutor identidade de tipo kont *) 
          then Pexp_construct ({txt = Lident "Kid"; loc = !(default_loc)}, None)
        else
          (* fun derivado da transformação CPS *)
          if pattHasANF_ then
              (* Criar construtor e retorno associado *)
              let expOfApply = restructPexpDescOfApply expression in
              let inExpression = 
                (* Valor de variável alterado em Pexp_ident, caso este seja kFunction. Caso seja
                significa que podemos resetar variável e fazer apply kFunction expression. Nos
                restantes casos não devemos fazer apply, mas sim substituir por um construtor *)
                if !applyHasKFunction then 
                    let () = applyHasKFunction := false in 
                    applyApply expOfApply 
                else 
                  (* Buscar info a lista de construtores. Não necessitamos do retorno associado *)
                  let (construtorName,construtoVars,_) = List.hd !listConstructorAndItsReturn in
                  let construtor = constructDefuncPexp construtorName construtoVars in
                  (* Expressão com construtor que será associada a construtor em apply, função
                  criada para processo de desfuncionalização *)
                  pexpDescOfApplyWithFunSubstitutedByConstruct 
                    expression 
                    construtor
              in
              (* Criar retorno de construtor e suas variáveis associadas *)
              let returnOfConstrutc = leTInExpressionOfDefuncConstructores pattern inExpression in
              let varOfConstruct = varsOfConstrutor expression (nameOfVarPattern pattern) in
              let () = 
                (* Criar lista de construtor e seu retorno, visando a criação de função
                apply com todos os construtores necessários. Adicionar a lista de construtores *)
                listConstructorAndItsReturn :=  
                (constructorName (!idOfConstructorsDefunc), 
                varOfConstruct,
                returnOfConstrutc 
                ) :: (!listConstructorAndItsReturn)
              in
              (* Incrementar valor de identificador de construtor, para a próxima iteração *)
              let () = idOfConstructorsDefunc := !idOfConstructorsDefunc + 1 in
              (* Buscar info a lista de construtores. Não necessitamos do retorno associado *)
              let (construtorName,construtoVars,_) = List.hd !listConstructorAndItsReturn in
              (* Construtor que ficará na função_aux. Anteriormente foi realizado trabalho para 
              incorporar construtores em apply, função criada aquando da incorporação de 
              desfuncionalização *)
              let construtorInAux = constructDefuncPexp construtorName construtoVars in
              construtorInAux
          else 
            (* Mantemos a estrutura de fun *)
            Pexp_fun (argLabel,expOption,pattern, newExpression)
      in
      let returnExp = {
        pexp_desc = returnPexpDesc;
        pexp_attributes = att; pexp_loc = loc
      } in 

      (* Repor valor de variável *)
      let () = funOfCps := beforeInFunOfCps in
      returnExp  
  (* Expressões não consideradas *)      
  | _ as expression -> expression

(* Tratar de matchCases e withCases (caso de try ... with Not_found, e.g.) *)
and changeMatchCasesDefuncionalization matchCasesList acc newFunctionName =
  match matchCasesList with 
  | [] -> List.rev acc
  | {pc_lhs = pcLhs ; pc_guard = pcGuard;
    pc_rhs =  matchContent} :: rx -> 
      let newMatchCase = 
        {
          pc_lhs = pcLhs; 
          pc_guard = pcGuard;
          pc_rhs =  changeExpressionToDefuncionalization newFunctionName matchContent
        } 
      in
      changeMatchCasesDefuncionalization rx (newMatchCase::acc) newFunctionName

(* Avalia pvbPatList: conteudo dentro de let in ou let and sucessivos *)

and changePvbPatListDefuncionalization newFunctionName acc = function
  | [] -> List.rev acc 
  | { pvb_pat = {ppat_desc = Ppat_var {txt = functionName; loc = loc};
      ppat_attributes = ppaAtt; ppat_loc = ppatLoc} as pvbPat; 
      pvb_expr = expression; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
    } ::rx -> 
    begin
      (* Avaliar se nome de função tem "_aux" e promover as alterações em caso afirmativo. Função
      com este nome provêm da transformação automática para CPS *)
      let funcNameHasAux = checkIfVarNamePattContainsPattern pvbPat "_aux" in
      (*let newFunctionName =
        if funcNameHasAux then functionName^"_def" else functionName in*)
      let pvbExpDefuncionalization =
        (* Retirar argumentos, para não interferir com avaliação *)
        let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
        (* Transformação para CPS *)
        let pvbExpDefunc = changeExpressionToDefuncionalization functionName pvbExpWithoutArguments in
        (* Retirar argumentos, para não interferir com avaliação *)
        let pvbExpDefuncwithArguments = changeBodyFunctionIgnoringArguments expression pvbExpDefunc in
        pvbExpDefuncwithArguments 
      in
      let tempDefunc = 
      {
          pvb_pat = {ppat_desc = Ppat_var {txt = functionName; loc = loc};
          ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
          pvb_expr = pvbExpDefuncionalization; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
      } 
      in
      (* Caso tenha no nome "_aux" então, deverá ser associado a esta função, com "and", a função
      apply, visando a desfuncionalização, com os construtores devidos. Neste momento já toda a
      função foi avaliada *)
      let returnList = 
        if funcNameHasAux then
          let rec createListConstructors acc = function
            | [] -> List.rev acc
            | el :: rx ->
              let (construtorName,construtorVars,returnExp) = el in
              (* Função que cria o construtor, com as suas variáveis e retorno associado a construtor *)
              let constructor = constructDefuncPpat construtorName construtorVars returnExp in
              createListConstructors (constructor::acc) rx 
          in
          let listConstructors = createListConstructors [] (!listConstructorAndItsReturn) in
          let returnExp = (applyFunction listConstructors) :: tempDefunc :: acc in 
          returnExp 
        else 
          tempDefunc :: acc
      in
      changePvbPatListDefuncionalization newFunctionName returnList rx
  end
  (* E.g Let () = ... *)  
  | { pvb_pat = {ppat_desc = Ppat_construct ({txt = Lident varName; loc = loc}, option);
      ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
      pvb_expr = expression; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
    } ::rx -> 
      let pvbExpDefuncionalization =
        (* Retirar argumentos, para não interferir com avaliação *)
        let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
        (* Transformação para CPS *)
        let pvbExpDefunc = changeExpressionToDefuncionalization newFunctionName pvbExpWithoutArguments in
        (* Retirar argumentos, para não interferir com avaliação *)
        let pvbExpDefuncwithArguments = changeBodyFunctionIgnoringArguments expression pvbExpDefunc in
        pvbExpDefuncwithArguments 
      in
      let tempDefunc =
      {
          pvb_pat = {ppat_desc = Ppat_construct ({txt = Lident varName; loc = loc}, option);
          ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
          pvb_expr = pvbExpDefuncionalization; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
      } 
      in 
      changePvbPatListDefuncionalization newFunctionName (tempDefunc::acc) rx
  | { pvb_pat = {ppat_desc = Ppat_tuple (tuppleList);
      ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
      pvb_expr = expression; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
    } ::rx -> 
      let pvbExpDefuncionalization =
        (* Retirar argumentos, para não interferir com avaliação *)
        let pvbExpWithoutArguments = returnFunctionIgnoringArguments expression in
        (* Transformação para CPS *)
        let pvbExpDefunc = changeExpressionToDefuncionalization newFunctionName pvbExpWithoutArguments in
        (* Retirar argumentos, para não interferir com avaliação *)
        let pvbExpDefuncwithArguments = changeBodyFunctionIgnoringArguments expression pvbExpDefunc in
        pvbExpDefuncwithArguments 
      in
      let tempDefunc = 
      {
        pvb_pat = {ppat_desc =  Ppat_tuple (tuppleList);
        ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
        pvb_expr = pvbExpDefuncionalization; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
      }   
      in
      changePvbPatListDefuncionalization newFunctionName (tempDefunc::acc) rx
  (* Não promover transformação *)
  | exp::rx -> changePvbPatListDefuncionalization newFunctionName (exp::acc) rx  
;;

(*************************************************************************************************)
(*                                                                                               *)
(*                              Fim de Defuncionalization                                        *)
(*                                                                                               *)
(*************************************************************************************************)

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
        (* Todas as alterações para CPS condensadas na função abaixo, visando reaproveitamento
        de código para desfuncionalization *)
        let newFunctionCPS = evalItemCPS label pvbList in
        default_mapper.structure_item mapper newFunctionCPS :: acc
    (* Caso a tag esteja associada a outra instrução que a estrutura contemplada, 
    remover a tag para que o código possa ser compilado (a tag não produzira alterações no código) *)
    | PStr [otherInstruction] -> default_mapper.structure_item mapper otherInstruction :: acc    
    (* Caso tenha tido a tag deverá ser do tipo PStr[...]. Este assert false é para garantir que
    o código nunca deverá chegar a este ponto *)
    | _ -> assert false
  end 
  (* Transformar em Defunc, usando a transformação para CPS como intermédia *)
  | { pstr_desc = Pstr_extension (({ txt = "Defunc"; _ }, pstr), _); _} -> 
  begin 
    match pstr with 
    (* Avaliar label e avaliaremos a sua expressão (pvb_expr) *)
    | (PStr [{ pstr_desc =
        Pstr_value (label,
          pvbList) 
      ; _} 
      ]) -> 
        (* Todas as alterações para CPS condensadas na função abaixo, visando reaproveitamento
        de código para desfuncionalization *)
        let newFunctionCPS = evalItemDefunc label pvbList in
        default_mapper.structure_item mapper newFunctionCPS :: acc
    (* Caso a tag esteja associada a outra instrução que a estrutura contemplada, 
    remover a tag para que o código possa ser compilado (a tag não produzira alterações no código) *)
    | PStr [otherInstruction] -> default_mapper.structure_item mapper otherInstruction :: acc    
    (* Caso tenha tido a tag deverá ser do tipo PStr[...]. Este assert false é para garantir que
    o código nunca deverá chegar a este ponto *)
    | _ -> assert false
  end 
  (* Avaliar try with para obter resultado de exceções *)
  | { pstr_desc = Pstr_value (_, pvbList) ; _} -> begin
      let () = evaluateTryWithPvbPatList pvbList in 
      default_mapper.structure_item mapper item :: acc 
  end 
  | _ -> default_mapper.structure_item mapper item :: acc 

(* Função que engloba todas as alterações (e as devidas chamdas a funções auxiliares) para 
transformar uma expressão em CPS, aquando da avaliação de um item (função anterior) *)
and evalItemCPS label pvbList = 
  (* Avaliar try with para obter resultado de exceções *)
  let () = evaluateTryWithPvbPatList pvbList in
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
  (* Verificar se é necessário remover rec label *)
  let label = 
    if !removeRecursiveTag 
      then let () = removeRecursiveTag := false in Nonrecursive 
      else label 
  in
  (* Nova função, com pvbList em formato CPS *)
  let newFunction = 
    { pstr_desc =
      Pstr_value (label, 
        cpsPvbList); 
      pstr_loc = !(default_loc) 
    }
  in 
  (* Retorna nova função/expressão transformada em CPS *)
  newFunction

(* Função, dentro da anterior, responsável por redirecionar transformações para função auxiliares,
alterandos as variáveis necessárias ao processo de trasnformação CPS *)
and transformExpToCPS label = function
  (* Estrutura para promover alteração para CPS *)
  | { pvb_pat = {
        ppat_desc = Ppat_var {txt = functionName; loc = loc};
        ppat_attributes = ppatAtt;
        ppat_loc = ppatLoc;
      } as pvbPat; 
      pvb_expr = pvbExpression; 
      pvb_attributes = pvbAtt; 
      pvb_loc = pvbLoc 
    } -> 
      begin
        (* Reset de variável *)
        let () = idException := 0 in
        let () = listExceptionAndReturn := [] in 
        let () = functionNumberOfExceptions := 0 in
        (* Avaliar se ambiente é recursivo ou não. Influencia transformação ANF, num caso
        particular, e transformações CPS *)
        let () = recEnvironment :=  
          match label with 
          | Recursive -> 
            (* Avaliação logo desta variável, caso let seja recursivo. Caso contrário
            será avaliado nos lets internos deste let. Evaluate exception permite verificar se,
            ao longo de toda a função, existe levantamento de exceção. Caso haja será
            adicionado um argumento extra e todas as chamadas recursivas à função terão de ter
            esta alteração em conta (daí ser feita uma função só para verificar esta existência) *)
            let () =  
              let temp = returnFunctionIgnoringArguments pvbExpression in
              let _ = evaluateException temp in ()
            in
            true 
          | Nonrecursive -> false
        in  
        (* Transformar em ANF *)   
        let newBodyFunction = changeFunctionToANF pvbExpression functionName in
        let newBodyFunctionCPS = 
          (* Avaliar aplicabilidade de funcao Aux in (explicação no topo da página) *)
          if (!recEnvironment) then 
            let () = applyFuncInAuxFromCPSTransfOnlyToExternalLetIn := true in
            (* Fornece a informação de que é necessário remover rec do let (recursividade ocorrerá
            dentro da funcaoAuxIn criada) *) 
            let () = removeRecursiveTag := false in 
            let newExp = changeFunctionToCPSWithFuncAuxIn newBodyFunction functionName pvbPat in
            newExp
          else
            let () = applyFuncInAuxFromCPSTransfOnlyToExternalLetIn := false in
            changeFunctionToCPS newBodyFunction functionName
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
      end
  | exp -> exp
(* Função que engloba todas as alterações para transformar uma expressão em CPS, reutilizando
funções anteriores, e, usando esta, promover alterações para defuncionalizar *)
and evalItemDefunc label pvbList = 
  (* Avaliar try with para obter resultado de exceções *)
  let () = evaluateTryWithPvbPatList pvbList in
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
  (* Verificar se é necessário remover rec label *)
  let label = 
    if !removeRecursiveTag 
      then let () = removeRecursiveTag := false in Nonrecursive 
      else label 
  in
  
  (* Defuncionalization *)
  let defuncPvbList = changePvbPatListDefuncionalization "xd" [] cpsPvbList in
  (* Nova função, com pvbList em formato CPS *)
  let newFunction = 
    { pstr_desc =
      Pstr_value (label, 
        defuncPvbList); 
      pstr_loc = !(default_loc) 
    }
  in 
  (* Retorna nova função/expressão transformada em CPS *)
  newFunction 

let structure_mapper mapper structure  =
  List.fold_right (eval_structure_item mapper) structure []
;;

let ppx_mapper _ =  
  { 
    default_mapper with 
    structure = structure_mapper;
  }

(* Avaliação preliminar das exceções presentes no programa. Ajuda a perceber se devemos retirar
try ... with ou não do programa. Caso a exceção presente neste não faça parte das declaradas no
programa então try ... with deve manter-se. Caso contrário deve remover-se try .. with pois o 
tratamento de exceções será feito dentro da função (por recurso a continuações de exceções) *)
let rec eval_exceptions mapper item acc =
  match item with
  | { pstr_desc = Pstr_exception
      {pext_name = {txt = exceptionName; _}; _}; _} -> begin
    let () = listExceptionFromUser := [exceptionName] @ !listExceptionFromUser in
    default_mapper.structure_item mapper item :: acc 
  end 
  | _ -> default_mapper.structure_item mapper item :: acc 

 
let structure_mapper_exceptions mapper structure  =
  List.fold_right (eval_exceptions mapper) structure []
;;

let ppx_mapper_exception _ =  
  { 
    default_mapper with 
    structure = structure_mapper_exceptions;
  }
 
let () = 
  (* Primeira avaliação para verificar a existência de exceções *)
  register "evaluateException" ppx_mapper_exception;
  (* Avaliação para transformação CPS *)
  register "ppxCPS" ppx_mapper




 