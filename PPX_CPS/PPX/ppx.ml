open Str
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Format 

(* Expressões auxiliares à injecção de código *)
let openFormat = 
{
  pstr_desc =
    Pstr_open {
      popen_lid = {txt = Lident "Format"; loc = !(default_loc)};
      popen_override = Fresh;
      popen_loc = !(default_loc);
      popen_attributes = []; 
    };   
  pstr_loc =  !(default_loc);
}  
;;

(* ---------- Função e instrucao (let aux = ref 0) auxiliar aos espaços no eprintf (formatação) ---------- *)

let printSpace =
{
  pstr_desc =
    Pstr_value 
      (Recursive,
        [{pvb_pat = {
            ppat_desc = Ppat_var {txt = "printSpace"; loc = !(default_loc)};
            ppat_attributes = [];
            ppat_loc = !(default_loc);
          };
          pvb_expr = { 
            pexp_desc =
              Pexp_fun 
                (Nolabel, None, {
                    ppat_desc = Ppat_var {txt = "n"; loc = !(default_loc)};
                    ppat_attributes = [];
                    ppat_loc = !(default_loc)
                  }, 
                {pexp_desc =
                  Pexp_ifthenelse
                  ({
                    pexp_desc =
                      Pexp_apply ({
                          pexp_desc = Pexp_ident {txt = Lident "<="; loc = !(default_loc)};
                          pexp_attributes = [];
                          pexp_loc = !(default_loc)
                        },
                      [(Nolabel, {
                          pexp_desc = Pexp_ident {txt = Lident "n"; loc = !(default_loc)};
                          pexp_attributes = [];
                          pexp_loc = !(default_loc)
                        });
                        (Nolabel,{
                          pexp_desc = Pexp_constant (Pconst_integer ("0", None));
                          pexp_attributes = [];
                          pexp_loc = !(default_loc) 
                        })
                      ]);
                    pexp_attributes= [];
                    pexp_loc = !(default_loc)
                    },
                  {
                    pexp_desc =
                      Pexp_apply (
                        {
                          pexp_desc = Pexp_ident {txt = Lident "eprintf"; loc = !(default_loc)};
                          pexp_attributes = [];
                          pexp_loc = !(default_loc)
                        },
                        [(Nolabel,
                          {
                            pexp_desc =
                              Pexp_constant (Pconst_string ("@[<hv 1> @]", None));
                            pexp_attributes = [];
                            pexp_loc = !(default_loc)
                          }
                        )]
                      );
                    pexp_attributes = [];
                    pexp_loc = !(default_loc)
                  },
                  Some
                    {
                      pexp_desc =
                        Pexp_sequence
                        ({
                          pexp_desc =
                            Pexp_apply
                            ({
                                pexp_desc = Pexp_ident {txt = Lident "eprintf"; loc = !(default_loc)};
                                pexp_attributes = [];
                                pexp_loc = !(default_loc)
                              },
                              [(Nolabel,
                                { 
                                  pexp_desc =
                                    Pexp_constant (Pconst_string ("@[<hv 1> @]", None));
                                  pexp_attributes = [];
                                  pexp_loc = !(default_loc)
                                }
                              )]
                            );
                            pexp_attributes = [];
                            pexp_loc = !(default_loc)
                          },
                        {
                          pexp_desc =
                            Pexp_apply
                              ({
                                  pexp_desc = 
                                    Pexp_ident {txt = Lident "printSpace"; loc = !(default_loc)};
                                  pexp_attributes = [];
                                  pexp_loc = !(default_loc) 
                                },
                                    [(Nolabel,
                                      {
                                        pexp_desc =
                                          Pexp_ifthenelse
                                          ({
                                              pexp_desc =
                                                Pexp_apply
                                                  ({
                                                    pexp_desc = Pexp_ident {txt = Lident ">"; loc = !(default_loc)};
                                                    pexp_attributes = [];
                                                    pexp_loc = !(default_loc)
                                                  },
                                                    [(Nolabel,
                                                      {
                                                        pexp_desc = Pexp_ident {txt = Lident "n"; loc = !(default_loc)};
                                                        pexp_attributes = [];
                                                        pexp_loc = !(default_loc)
                                                      });
                                                      (Nolabel,
                                                      {
                                                        pexp_desc =
                                                          Pexp_constant (Pconst_integer ("2", None));
                                                        pexp_attributes = [];
                                                        pexp_loc = !(default_loc)
                                                      }
                                                    )]
                                                  );
                                              pexp_attributes = [];
                                              pexp_loc = !(default_loc)  
                                            },
                                          {
                                            pexp_desc =
                                              Pexp_constant (Pconst_integer ("2", None));
                                            pexp_attributes = [];
                                            pexp_loc = !(default_loc)
                                          },
                                              Some
                                                {
                                                  pexp_desc =
                                                    Pexp_apply
                                                    ({
                                                      pexp_desc = Pexp_ident {txt = Lident "-"; loc = !(default_loc)};
                                                      pexp_attributes = [];
                                                      pexp_loc = !(default_loc)
                                                    },
                                                    [(Nolabel,
                                                      {
                                                        pexp_desc = Pexp_ident {txt = Lident "n"; loc = !(default_loc)};
                                                        pexp_attributes = [];
                                                        pexp_loc = !(default_loc)
                                                      }
                                                    );
                                                      (Nolabel,
                                                      {
                                                        pexp_desc =
                                                          Pexp_constant (Pconst_integer ("1", None));
                                                        pexp_attributes = [];
                                                        pexp_loc = !(default_loc)
                                                      })
                                                    ]
                                                    );
                                                  pexp_attributes = [];
                                                  pexp_loc = !(default_loc)
                                                }
                                          );
                                          pexp_attributes = [];
                                          pexp_loc = !(default_loc)
                                      }
                                  )]
                                );
                          pexp_attributes = [];
                          pexp_loc = !(default_loc)
                        }
                        );
                      pexp_attributes = [];
                      pexp_loc = !(default_loc)
                    }
                  );
                pexp_attributes = [];
                pexp_loc = !(default_loc)
                }
                );
            pexp_attributes = [];
            pexp_loc = !(default_loc)
          };
          pvb_attributes = [];
          pvb_loc = !(default_loc)
        }]
      );
  pstr_loc = !(default_loc);
}
;;

let auxVariavelEspacos =
{
  pstr_desc =
   Pstr_value 
    (Nonrecursive,
      [{
        pvb_pat = 
          {
            ppat_desc = Ppat_var {txt = "auxVariavelSemNome"; loc = !(default_loc)};
            ppat_attributes = [];
            ppat_loc = !(default_loc)
          }; 
        pvb_expr =
          {
            pexp_desc =
              Pexp_apply 
              ({
                  pexp_desc = 
                    Pexp_ident {txt = Lident "ref"; loc = !(default_loc)};
                  pexp_attributes = [];
                  pexp_loc = !(default_loc)
                },
                  [(Nolabel,
                    {
                      pexp_desc = Pexp_constant (Pconst_integer ("0", None));
                      pexp_attributes = [];
                      pexp_loc = !(default_loc)
                    }
                  )]
              );
            pexp_attributes = [];
            pexp_loc = !(default_loc)
          };
        pvb_attributes = [];
        pvb_loc = !(default_loc)
      }]
    );
  pstr_loc = !(default_loc);
}
;;
(* ------------------------------------------------------------------------------------- *)

let eprintExp stmt arg =
{
  pexp_desc =
    Pexp_apply ({
      pexp_desc = Pexp_ident {txt = Lident "eprintf"; loc = !(default_loc)};
      pexp_loc = !(default_loc);
      pexp_attributes = []
      },
      [(Nolabel, {
        pexp_desc =
          Pexp_constant (Pconst_string (stmt, None));
        pexp_loc = !(default_loc);
        pexp_attributes = [];
        });
        (Nolabel, {
          pexp_desc = 
            Pexp_ident {txt = Lident arg; loc = !(default_loc)};
          pexp_loc = !(default_loc);
          pexp_attributes = [];
        });
      ]
    );
  pexp_loc = !(default_loc);
  pexp_attributes = []
}
;;

(* if then para imprimir "Entrei em funcao" *)
let ifthenExp expression =
{
      pexp_desc =
       Pexp_ifthenelse
        ({
          pexp_desc =
            Pexp_apply (
              {
                pexp_desc = Pexp_ident {txt = Lident "="; loc = !(default_loc)};
                pexp_attributes = [];
                pexp_loc = !(default_loc)
              },
              [
                (Nolabel,{
                  pexp_desc =
                    Pexp_apply (
                      {
                        pexp_desc = Pexp_ident {txt = Lident "!"; loc = !(default_loc)};
                        pexp_attributes = [];
                        pexp_loc = !(default_loc)
                      },
                      [(Nolabel, { 
                          pexp_desc = Pexp_ident {txt = Lident "auxVariavelSemNome"; loc = !(default_loc)};
                          pexp_attributes = [];
                          pexp_loc = !(default_loc)
                        }
                      )]
                    );
                  pexp_attributes = [];
                  pexp_loc = !(default_loc)
                }
                );
                (Nolabel,
                  {
                    pexp_desc = Pexp_constant (Pconst_integer ("0", None));
                    pexp_attributes = [];
                    pexp_loc = !(default_loc)
                  }
              )]
            );
            pexp_attributes = [];
            pexp_loc = !(default_loc)
          },
        expression,
        None);
      pexp_attributes = [];
      pexp_loc = !(default_loc)
}

(* Chamada da funcao de formataçao de espaços *)
let callPrintSpace =
{
  pexp_desc =
    Pexp_apply 
    ({
        pexp_desc = 
          Pexp_ident {txt = Lident "printSpace"; loc = !(default_loc)};
        pexp_attributes = [];
        pexp_loc = !(default_loc)
      },
      [(Nolabel,
        {
          pexp_desc = 
            Pexp_apply ({
                pexp_desc = 
                  Pexp_ident {txt = Lident "!"; loc = !(default_loc)};
                pexp_attributes = [];
                pexp_loc = !(default_loc)
              },
              [(Nolabel,
                {
                  pexp_desc = 
                    Pexp_ident {txt = Lident "auxVariavelSemNome"; loc = !(default_loc)};
                  pexp_attributes = [];
                  pexp_loc = !(default_loc)
                }
              )]
            );
          pexp_attributes = [];
          pexp_loc = !(default_loc)
        }
      )]
     );
  pexp_attributes = [];
  pexp_loc = !(default_loc)
}

(* Incremento de variável auxiliar na formatação de espaços *)
let incrementaVariavelAux =
{
  pexp_desc =
    Pexp_apply 
    ({
        pexp_desc = 
          Pexp_ident {txt = Lident ":="; loc = !(default_loc)};
        pexp_attributes = [];
        pexp_loc = !(default_loc)
      },
      [(Nolabel,
        {
          pexp_desc = 
            Pexp_ident {txt = Lident "auxVariavelSemNome"; loc = !(default_loc)};
          pexp_attributes = [];
          pexp_loc = !(default_loc)
        }
      );
      (Nolabel,{
          pexp_desc = 
            Pexp_apply ({
                pexp_desc = Pexp_ident {txt = Lident "+"; loc = !(default_loc)};
                pexp_attributes = [];
                pexp_loc = !(default_loc)
              },
              [(Nolabel,
                {
                  pexp_desc =
                    Pexp_apply (
                      {
                        pexp_desc = Pexp_ident {txt = Lident "!"; loc = !(default_loc)};
                        pexp_attributes = [];
                        pexp_loc = !(default_loc)
                      },
                        [(Nolabel,
                            {
                              pexp_desc =
                                Pexp_ident {txt = Lident "auxVariavelSemNome"; loc = !(default_loc)};
                              pexp_attributes = [];
                              pexp_loc = !(default_loc)
                            }
                        )]
                    );
                  pexp_attributes = [];
                  pexp_loc = !(default_loc)
                }
              );
              (Nolabel,
                {
                  pexp_desc =
                    Pexp_constant (Pconst_integer ("1", None));
                  pexp_attributes = [];
                  pexp_loc = !(default_loc)
                }
              )]
                
            );
            pexp_attributes = [];
            pexp_loc = !(default_loc)
            
            });
      ]
     );
  pexp_attributes = [];
  pexp_loc = !(default_loc)
}

(* Projeções de variaveis de record *)
let recordProjection recordName variableName =
let projVariableRecord = "proj_"^variableName^"_of_"^recordName in
{
  pstr_desc =
    Pstr_value (Nonrecursive,
    [{
      pvb_pat = {
        ppat_desc = Ppat_var {txt = projVariableRecord; loc = !(default_loc)};
        ppat_attributes = []; 
        ppat_loc = !(default_loc)
      };
      pvb_expr =
      {
        pexp_desc =
          Pexp_fun (Nolabel, None,
            {
              ppat_desc =
                Ppat_constraint ({
                    ppat_desc = 
                      Ppat_var {txt = recordName; loc = !(default_loc)};
                    ppat_attributes = [];
                    ppat_loc = !(default_loc)
                  },
                  {
                    ptyp_desc = 
                      Ptyp_constr ({txt = Lident recordName; loc = !(default_loc)}, []);
                    ptyp_attributes = [];
                    ptyp_loc = !(default_loc)
                  }
                );
              ppat_attributes = [];
              ppat_loc = !(default_loc)
            },
            {
              pexp_desc =
                Pexp_field ({
                    pexp_desc = Pexp_ident {txt = Lident recordName; loc = !(default_loc)};
                    pexp_attributes = [];
                    pexp_loc = !(default_loc)
                  },
                  {txt = Lident variableName; loc = !(default_loc)}
                );
              pexp_attributes = [];
              pexp_loc = !(default_loc)
            }
          );
        pexp_attributes = [];
        pexp_loc = !(default_loc)
      };
      pvb_attributes = [];
      pvb_loc = !(default_loc)  
    }]);
  pstr_loc = !(default_loc)
}     


let sequenceExpression instrA instrB =
{
  pexp_desc = Pexp_sequence (instrA,instrB);
  pexp_loc = !(default_loc);
  pexp_attributes = []
}
;;   

(* --------------------------------------------------------------------- *)

(* Auxiliar à construção de let ... in *)

(* Usada mais em baixo (newMatchCaseWithLetIn) *)
let auxRefVarNameLetIn = ref 0

let letInExpressionFunction functionName inExpression restBodyFunctionExp =
  let () = (auxRefVarNameLetIn := !(auxRefVarNameLetIn) - 1) in 
  let varName = functionName^"ANF_"^(string_of_int (!auxRefVarNameLetIn)) in
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

(* Altera o nome de variáveis para condizer com alterações de let in *)
let rec alteraExp exp = function
  | [] -> exp
  | (el, varName)::rx -> if el = exp then makeVar varName else alteraExp exp rx

(* Altera o nome de variáveis para condizer com alterações de let in *)
let rec alteraLetInExp expression listExpLetIn =
  match expression with
  (* O caso mais comum *)
  | {pexp_desc = Pexp_apply (
      {pexp_desc = Pexp_ident {txt = Lident varName; _}; _}, 
      i2); _
    } as expression -> 
    let temp = alteraExp expression listExpLetIn in
    if expression <> temp then temp else 
    begin 
      let rec aux acc = function
        | [] -> List.rev acc
        | (_ as label ,i) :: rx -> 
          (* CRUCIAL *)
          let temp = alteraExp (alteraLetInExp i listExpLetIn) listExpLetIn in 
          aux ((label,temp)::acc) rx
      in
      {
        pexp_desc = Pexp_apply (
          { pexp_desc = Pexp_ident {txt = Lident varName; loc = !(default_loc)}; 
            pexp_attributes = [];
            pexp_loc = !(default_loc)
          }, 
        (aux [] i2));
        pexp_attributes = [];
        pexp_loc = !(default_loc)
      } 
    end
  (* Promove as alterações ITE, mediante o determinado por hasRecCall *)
  | {pexp_desc = Pexp_ifthenelse (ifExp, thenExp, Some(elseExp)); pexp_attributes = att; pexp_loc = loc} ->
    {pexp_desc = Pexp_ifthenelse (
      alteraLetInExp ifExp listExpLetIn, 
      alteraLetInExp thenExp listExpLetIn, 
      Some(alteraLetInExp elseExp listExpLetIn));
    pexp_attributes = att; 
    pexp_loc = loc}
  (* Particular *) 
  | {pexp_desc = Pexp_tuple [pexp1;pexp2]; pexp_attributes = att; pexp_loc = loc} 
    -> {pexp_desc = 
          Pexp_tuple [
            alteraLetInExp pexp1 listExpLetIn;
            alteraLetInExp pexp2 listExpLetIn
          ]; 
       pexp_attributes = att; pexp_loc = loc} 
  | _ as otherExp -> (* eprintf "Valor: %s@." (Pprintast.string_of_expression otherExp); *) otherExp

(* Altera match cases para ter formato ANF (let in transfomation) *)
let rec newMatchCaseWithLetIn expression functionName listLetIn = function
  | [] -> (alteraLetInExp expression listLetIn) 
  | el :: rx -> let varNameOfEl = functionName^"ANF_"^(string_of_int (!auxRefVarNameLetIn)) in
                let () = (auxRefVarNameLetIn := !auxRefVarNameLetIn + 1) in
                let newExp = (newMatchCaseWithLetIn expression functionName ((el,varNameOfEl)::listLetIn) rx) in
                letInExpressionFunction functionName el newExp
                
;;

(* A função que determina as alterações para let in em todos os casos considerados *)
let rec hasRecCall (expression : Parsetree.expression) (functionName : string) acc = 
  match expression with
  | {pexp_desc = Pexp_apply (
      {pexp_desc = Pexp_ident {txt = Lident varName; _}; _}, 
      expList); _
    } as auxExp ->  
      begin 
        let rec aux acc = function
        | [] -> List.rev acc
        | (_, exp) :: rx -> 
          (* CRUCIAL *)
          if varName = functionName then ((auxExp)::acc) 
          else aux (hasRecCall exp functionName acc) rx
        in aux acc expList 
      end
  (* Particular *)
  | {pexp_desc = Pexp_tuple [pexp1;pexp2]; _} 
    -> 
      let newAcc = hasRecCall pexp1 functionName acc in
      hasRecCall pexp2 functionName newAcc
  (* Não é neste match case que se encontra a chamada recursiva à função *) 
  | _ -> acc


(* Auxiliar para retornar corpo da função, sem argumentos; tb usada em transformação ANF *)
let rec returnFunctionIgnoringArguments currentFunction  = 
  match currentFunction with
    (* Ainda é argumento *)
    | {pexp_desc = Pexp_fun (_,_,_,expression); _} -> 
      returnFunctionIgnoringArguments expression
    (* Não é argumento, é corpo da função (o que é devolvido) *) 
    | {pexp_desc = _; _} as bodyFunction -> bodyFunction
;; 

(* Auxiliar à alteração de corpo de funções, sem avaliar argumentos; tb usada em transformação ANF *) 
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
    | {pexp_desc = _; _} -> 
      newBodyFunction
;;

(* Auxiliar para perceber se estamos no caso function (temo de alterar para match para manter
estrutura de CPS) ou não e se é necessário adicionar argumento extra na transformação 
de function para match *) 
let changeFunctionToMatch = ref false

let rec evaluateExpression functionName = function
    (* Tratar de let in's de ITE, individualmente *)
    | {pexp_desc = Pexp_ifthenelse (ifExp, thenExp, Some(elseExp)); 
      pexp_attributes = att; pexp_loc = loc } ->
        (* avaliar ITE aninhados *)
        let letInListIfExp = evaluateExpression functionName ifExp in
        let letInListThenExp = evaluateExpression functionName thenExp in
        let letInListElseExp = evaluateExpression functionName elseExp in
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
        let letInListIfExp = evaluateExpression functionName ifExp in
        let letInListThenExp = evaluateExpression functionName thenExp in
        {
          pexp_desc = Pexp_ifthenelse (
          letInListIfExp, 
          letInListThenExp,  
          None); 
          pexp_attributes = att; pexp_loc = loc 
        }
    (* function = |.. -> .. *)
    | {pexp_desc = Pexp_function(listMatchCases); pexp_attributes = att; pexp_loc = loc } ->
      (* Converter todas as function to match ... with *)
        let () = changeFunctionToMatch := true in
        let matchArg = {pexp_desc = Pexp_ident 
          {txt = Lident "matchArg"; loc = !(default_loc)}; 
          pexp_attributes = []; 
          pexp_loc = !(default_loc)
        } in
        {pexp_desc = (Pexp_match (matchArg, (changeExpLetin listMatchCases functionName [])));
          pexp_attributes = []; 
          pexp_loc = !(default_loc)
        }
      (* Match ... with aninhados  *)
    | {pexp_desc = Pexp_match (matchArg ,listMatchCases); pexp_attributes = att; pexp_loc = loc } ->
        let letInListMatch = changeExpLetin listMatchCases functionName [] in
        {
          pexp_desc = Pexp_match (
          matchArg, 
          letInListMatch); 
          pexp_attributes = att; pexp_loc = loc 
        }
    (* Caso de let in de utilizador *)
    | {pexp_desc = Pexp_let (tag, 
        [{pvb_pat = varName; pvb_expr = exp; pvb_attributes = att; pvb_loc = loc}], 
        nextExp 
        ); pexp_attributes = pAtt; pexp_loc = pLoc 
      } -> 
      (* Consideramos o caso que com let nomeFunc arg1 arg2 ... = ... in, iremos ignorar os 
      argumentos e tratar do corpo da função. O caso de let temp = 2 in (E.G) decorre sem 
      incidentes/alterações *) 
        let letinExp = returnFunctionIgnoringArguments exp in
        (* avaliar o corpo da função, sem argumento *)
        let letInListExp = evaluateExpression functionName letinExp in
        let letInListNextExp = evaluateExpression functionName nextExp in
        (* Devolver os argumentos à nova expressão let in *)
        let newLetInExpWithOldArg = changeBodyFunctionIgnoringArguments exp letInListExp in
        { 
          pexp_desc = Pexp_let (tag,
          [{pvb_pat = varName; pvb_expr = newLetInExpWithOldArg; 
            pvb_attributes = att; pvb_loc = loc}], 
          letInListNextExp); 
          pexp_attributes = pAtt; pexp_loc = pLoc 
        } 
    (* Sequencia  *) 
    | {pexp_desc = Pexp_sequence(exp1, exp2);
        pexp_attributes = pAtt; pexp_loc = pLoc 
      } -> 
        let letInListExp1 = evaluateExpression functionName exp1 in
        let letInListExp2 = evaluateExpression functionName exp2 in
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
        let letInListExp = evaluateExpression functionName exp in
        (* Not_found tem a estrutura de matchCases *)
        let letInListWithList = changeExpLetin withList functionName [] in
        {
          pexp_desc = Pexp_try(letInListExp, letInListWithList);
          pexp_attributes = pAtt; pexp_loc = pLoc 
        }
    (* Caso de :: (e.g) usado em listas (List) ou N(...) em árvores *)
    | {pexp_desc = Pexp_construct(lident, Some(exp));
        pexp_attributes = pAtt; pexp_loc = pLoc 
      } ->
        let newExp = evaluateExpression functionName exp in
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
            let evaluateEl = evaluateExpression functionName el in
            evaluateTupleList (evaluateEl::acc) rx
        in         
        let newTupleList = evaluateTupleList [] tupleList in
        {
          pexp_desc = Pexp_tuple(newTupleList);
          pexp_attributes = pAtt; pexp_loc = pLoc 
        }
    (* Caso de aplicação *)
    (*| {pexp_desc = Pexp_apply(pexp, applyList);
        pexp_attributes = pAtt; pexp_loc = pLoc 
      } ->  
        let rec evaluateApplyList acc = function
          | [] -> List.rev acc
          | (label, exp) 
            :: rx ->   
              let evaluateExp = evaluateExpression functionName exp in
              evaluateApplyList ((label,evaluateExp)::acc) rx
        in   
        let newPexpExp = evaluateExpression functionName pexp in
        let newApplyList = evaluateApplyList [] applyList in
        {
          pexp_desc = Pexp_apply(newPexpExp,newApplyList);
          pexp_attributes = pAtt; pexp_loc = pLoc 
        }*)
    (* Outros casos (aplicação/base) *)
    | _ as exp ->  
        let letInList = hasRecCall exp functionName [] in 
        (newMatchCaseWithLetIn exp functionName [] (List.rev letInList))

(* Tratar de matchCases e withCases (caso de try ... with Not_found, e.g.) *)
and changeExpLetin matchCasesList functionName acc =
  match matchCasesList with 
  | [] -> List.rev acc
  | {pc_lhs = pcLhs ; pc_guard = pcGuard;
    pc_rhs =  matchContent} :: rx -> 
      let newMatchCase = 
        {
          pc_lhs = pcLhs; 
          pc_guard = pcGuard;
          pc_rhs =  evaluateExpression functionName matchContent
        } 
      in
      changeExpLetin rx functionName (newMatchCase::acc)

(* --------------------------------------------------------------------------- *)

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

(* Auxiliar para retirar tag recursiva a funcao *)
let removeRecursiveTag = function
  | { pstr_desc = Pstr_value (Recursive, valuesBinding);
      pstr_loc = local;
    } -> 
      { pstr_desc = Pstr_value (Nonrecursive, valuesBinding);
      pstr_loc = local;} 
  | _ -> assert false
;;

(* Auxiliar para tornar reunir todos os argumentos da função principal e a passar para funcaoAux in *)
let gatherArgsToInFunctionAuxAndAddFuncIdent currentFunction =
  let rec gatherArgsToInFunctionAuxAndAddFuncIdent_Aux acc = function
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
      gatherArgsToInFunctionAuxAndAddFuncIdent_Aux (argsFunction::acc) expression
    (* Não é argumento, é corpo da função. Podemos devolver a lista acc, 
    depois de adicionarmos fun s -> s *) 
    | {pexp_desc = _; _} -> 
    let funIdent = 
      (Nolabel,
      {pexp_desc = 
        Pexp_fun (Nolabel, None,
          {ppat_desc = Ppat_var {txt = "s"; loc = !(default_loc)}; ppat_attributes = []; ppat_loc = !(default_loc)},
          {pexp_desc = Pexp_ident {txt = Lident "s"; loc = !(default_loc)}; pexp_attributes = []; pexp_loc = !(default_loc;)}
        );
      pexp_attributes = [];
      pexp_loc = !(default_loc);
      }) 
    in 
    (* Devolução final *)
    List.rev (funIdent::acc)
  in 
  gatherArgsToInFunctionAuxAndAddFuncIdent_Aux [] currentFunction
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
(* Perceber se devemos aplicar k (fun s -> s) à expressão. Por omissão sim, pois o primeiro
match case tendo a ser o que finaliza a função. Alterado para false, depois de ser aplicado a 
primeira vez, e apenas é transformado para true caso haja restructPexpLetInToCPS *)
let applyK = ref true

(* Auxiliar para percorrer todos os argumentos da aplicaçao da chamada recursiva em let in, chegar
ao fim da lista e adicionar o fun (varName -> ...), chamando depois changeExpressionLetInsToCPSStyle *)
let rec restructPexpLetInToCPS newFunctionName varName restExpression = function
  | [] ->
    let () = applyK := true in
    [(Nolabel, {
    pexp_desc = 
      Pexp_fun (Nolabel, None, varName, (changeExpressionLetInsToCPSStyle newFunctionName restExpression));
    pexp_attributes = [];
    pexp_loc = !(default_loc) 
    })]
  | (label, exp)::rx -> [(label, exp)]@(restructPexpLetInToCPS newFunctionName varName restExpression rx)

(* Converter todos os let in's  em .... (fun ... -> ), CPS style
  É garantido que, ao chegar a esta função, a expressão já está em ANF *)
and changeExpressionLetInsToCPSStyle newFunctionName = function
    (* Let in que será transformado em exp (fun varName -> ...) *)
    | {pexp_desc = Pexp_let (
        tag, 
        [{pvb_pat = varNamePatt; pvb_expr = pvbExp; pvb_attributes = pvbAtt; pvb_loc = pvbLoc}], 
        nextExp
      ); pexp_attributes = pexpAtt; pexp_loc = pexpLoc
      } -> 
      begin 
        (* Verificar se depois de let in temos aplicação com nome de variavel criado por ppx
        => functionName+ANF_#id, o que significa que teremos promover alteração para CPS, nestes casos *)
        match pvbExp with
        | {pexp_desc = Pexp_apply (pexp, list); pexp_attributes = att; pexp_loc = loc}
          -> 
          (* Applicação provém de um let in, oriundo de transformação ANF *)
          if (checkIfApplicationComesFromANFTransformation varNamePatt) then
            {
              pexp_desc = Pexp_apply  
                ((changeFuncNameInLetInExp newFunctionName pexp), 
                (restructPexpLetInToCPS newFunctionName varNamePatt nextExp list)
                );  
              pexp_attributes = att; 
              pexp_loc = loc 
            }
          (* Applicação não provém de um let in, oriundo de transformação ANF, logo tratar como
          se fosse qualquer outra expressão ( | _ -> ... ) *)
          else 
            changeLetInExpFromUserAndNotFromANFTransformation
              newFunctionName tag varNamePatt pvbAtt pvbLoc pexpAtt pexpLoc pvbExp nextExp
        (* Let in do utilizador, avaliamos nextExp. Consideramos aqui ignorar tb os args de let 
        caso este os tenha (como em changeExpLetin) *)
        | _ as exp ->   
            changeLetInExpFromUserAndNotFromANFTransformation
              newFunctionName tag varNamePatt pvbAtt pvbLoc pexpAtt pexpLoc exp nextExp
      end
    (* Aplicação *)
    | {pexp_desc = Pexp_apply (pexp, list); pexp_attributes = att; pexp_loc = loc} as appExp
        -> appExp
    (* ITE, if não é aplicado CPS *)
    | {pexp_desc = Pexp_ifthenelse (ifExp, thenExp, Some(elseExp)); 
        pexp_attributes = att; pexp_loc = loc} 
        -> {
          pexp_desc = Pexp_ifthenelse (
            (* changeExpressionLetInsToCPSStyle newFunctionName ifExp *)
            ifExp, 
            changeExpressionLetInsToCPSStyle newFunctionName thenExp, 
            Some(changeExpressionLetInsToCPSStyle newFunctionName elseExp)
            );  
            pexp_attributes = att; pexp_loc = loc
        }
    (* IT *)
    | {pexp_desc = Pexp_ifthenelse (ifExp, thenExp, None); 
        pexp_attributes = att; pexp_loc = loc} 
        -> {
          (* changeExpressionLetInsToCPSStyle newFunctionName ifExp *)
          pexp_desc = Pexp_ifthenelse (
            ifExp, 
            changeExpressionLetInsToCPSStyle newFunctionName thenExp, 
            None
            ); 
            pexp_attributes = att; pexp_loc = loc
        }
    (* function = ... *)
    | {pexp_desc = (Pexp_function (matchCasesList));
        pexp_attributes = att; 
        pexp_loc = loc
      } -> 
        {pexp_desc = (Pexp_function (
            changeMatchCase newFunctionName matchCasesList []));
          pexp_attributes = att; 
          pexp_loc = loc
        }
    (* Match case aninhados *)
    | {pexp_desc = (Pexp_match (matchArg, matchCasesList));
        pexp_attributes = att; 
        pexp_loc = loc
      } -> 
        {pexp_desc = (Pexp_match 
            (matchArg,  
            changeMatchCase newFunctionName matchCasesList []));
          pexp_attributes = att; 
          pexp_loc = loc
        }
    (* Avaliar sequência *)
    | {pexp_desc = (Pexp_sequence (exp1, exp2)); pexp_attributes = att; pexp_loc = loc}
       -> {
            pexp_desc = (Pexp_sequence (
              changeExpressionLetInsToCPSStyle newFunctionName exp1, 
              changeExpressionLetInsToCPSStyle newFunctionName exp2)
            ); 
            pexp_attributes = att; 
            pexp_loc = loc
          }
    (* Avaliar try ... with Not_found (e.g) *)
    | {pexp_desc = (Pexp_try (exp, withList)); pexp_attributes = att; pexp_loc = loc}
       -> {
            pexp_desc = (Pexp_try (
              changeExpressionLetInsToCPSStyle newFunctionName exp,
              (* withList tem mesma estrutura de mathCase *) 
              changeMatchCase newFunctionName withList [])
            ); 
            pexp_attributes = att; 
            pexp_loc = loc
          }
    (* Caso de :: (e.g), usado em listas (List) *)
    | {pexp_desc = Pexp_construct(lident, Some(exp));
        pexp_attributes = pAtt; pexp_loc = pLoc 
      } ->
        let newExp = changeExpressionLetInsToCPSStyle newFunctionName exp in
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
            let evaluateEl = changeExpressionLetInsToCPSStyle newFunctionName el in
            evaluateTupleList (evaluateEl::acc) rx
        in         
        let newTupleList = evaluateTupleList [] tupleList in
        {
          pexp_desc = Pexp_tuple(newTupleList);
          pexp_attributes = pAtt; pexp_loc = pLoc 
        }
    (* Final de let in ou exemplo de expressão sem let in. Adicionar k (fun s -> s) a esta expressão *)               
    | _ as expression -> 
    begin 
      if (!applyK) then
      begin
          let () = applyK := true in (* Avaliar situação ... *)
          (* Adição de k (fun s -> s) *)
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
      end 
      else expression 
    end
 
(* Auxiliar para avaliar cada match case e promever às suas alterações para CPS *)
and changeMatchCase newFunctionName matchCasesList acc =
  match matchCasesList with 
  | [] -> List.rev acc
  | {pc_lhs = pcLhs ; pc_guard = pcGuard;
    pc_rhs =  matchContent} :: rx -> 
      begin
        (* Resetar variável *)
        (* let () = applyK := true in , Avaliar situação ...*)
        let newMatchCase =  
          {
            pc_lhs = pcLhs;  
            pc_guard = pcGuard;
            pc_rhs =  (changeExpressionLetInsToCPSStyle newFunctionName matchContent)
          } 
        in
        changeMatchCase newFunctionName rx (newMatchCase::acc)
      end
(* Alteração de let exp in nextExp, não criadas por ppx (oriundas do user). Não são alteradas, mas o
conteudo dentro de exp e nextExp pode ser, daí ter de ser avaliado. Criada para evitar repetição de
código nos casos de else em apply (detro de let int) e _ (ambos dentro do mesmo match case de let in) *)
and changeLetInExpFromUserAndNotFromANFTransformation 
  newFunctionName tag varNamePatt pvbAtt pvbLoc pexpAtt pexpLoc exp nextExp =
  begin
    let letinExp = returnFunctionIgnoringArguments exp in
    (* avaliar o corpo da função, sem argumento *)
    let letInExpCPS = changeExpressionLetInsToCPSStyle newFunctionName letinExp in
    (* Devolver os argumentos à nova expressão let in *)
    let letInExpCPSWithOldArg = changeBodyFunctionIgnoringArguments exp letInExpCPS in
    {pexp_desc = Pexp_let (  
      tag, 
      [{pvb_pat = varNamePatt; pvb_expr = letInExpCPSWithOldArg; 
      pvb_attributes = pvbAtt; pvb_loc = pvbLoc}], 
      changeExpressionLetInsToCPSStyle newFunctionName nextExp
      );
      pexp_attributes = pexpAtt; pexp_loc = pexpLoc
    }
  end

(* Estrutura de ITE *)
let iteExpStructure ifExp thenExp elseExp att loc =
{
  pexp_desc = Pexp_ifthenelse (
    (ifExp),        
    (thenExp),     
    Some(elseExp)
  );   
  pexp_attributes = att; 
  pexp_loc = loc;
}

let itExpStructure ifExp thenExp att loc =
{
  pexp_desc = Pexp_ifthenelse (
    (ifExp),        
    (thenExp),     
    None
  );   
  pexp_attributes = att; 
  pexp_loc = loc;
}


(* Auxiliar para introducao argumento exta: fun s -> s e alterar o corpo da função para que
os match cases contenham o argumento exta k (fun s -> s)*)
let rec addArgument newFunctionName = function
  (* Ainda é argumento *)
  | {pexp_desc = Pexp_fun (argLabel,expOption,pattern,expression); _} -> 
      {
        pexp_desc = Pexp_fun (argLabel,expOption,pattern, 
          addArgument newFunctionName expression);
        pexp_attributes = []; 
        pexp_loc = !(default_loc);
      } 
  (* Não é argumento, é corpo da função. Adicionar o argumento k (fun s -> s) *) 
  | {pexp_desc = _; _} as bodyFunction -> 
      {
        pexp_desc =
          Pexp_fun (Nolabel, None,
            {
              ppat_desc = Ppat_var {
                txt = "kFunction"; loc = !(default_loc)
              };
              ppat_attributes = [];
              ppat_loc = !(default_loc);
            }, 
          changeExpressionLetInsToCPSStyle newFunctionName bodyFunction
        );
        pexp_attributes = [];
        pexp_loc = !(default_loc);
      } 
;;

let createFuncAuxIn = function
  | { pstr_desc = Pstr_value (label, 
    [{pvb_pat = 
      {
        ppat_desc = Ppat_var {txt = functionName; loc = varLoc};
        ppat_attributes = ppatAtt;
        ppat_loc = ppatLoc;
      };
      pvb_expr = expression;
      pvb_attributes = att;
      pvb_loc = loc
    }]);
      pstr_loc = local;
    } -> 
    let functionAuxName = functionName^"_aux" in
    begin  
      {pexp_desc = Pexp_let 
        (label,
        (* Corpo da funcao (let rec height aux =  ...) *) 
        [
          {pvb_pat = {
            ppat_desc = Ppat_var {txt = functionAuxName; loc = varLoc};
            ppat_attributes = ppatAtt;
            ppat_loc = ppatLoc;
          };
          (* Adicionar o argumento extra (fun s -> s) *)
          pvb_expr = addArgument functionAuxName expression;
          pvb_attributes = att;
          pvb_loc = loc}
        ],
        (* Argumentos de funcao (... in tree (fun s -> s)) *)
        { pexp_desc = Pexp_apply 
          ({pexp_desc = Pexp_ident {txt = Lident functionAuxName; loc = !(default_loc)};
            pexp_attributes = [];
            pexp_loc = !(default_loc);
          }, (gatherArgsToInFunctionAuxAndAddFuncIdent expression)
        ); 
          pexp_attributes = [];
          pexp_loc = local;
        });
      pexp_attributes = []; pexp_loc = loc;
      }
    end
  | _ as e ->  eprintf "Valor: %s@." (Pprintast.string_of_structure [e]); assert false

(* Alteração para CPS em let rec ... in internos *)
let createFuncAuxInInteriorLetIn nextExp = function
  | { pexp_desc = Pexp_let (label, 
    [{pvb_pat = 
      {
        ppat_desc = Ppat_var {txt = functionName; loc = varLoc};
        ppat_attributes = ppatAtt;
        ppat_loc = ppatLoc;
      };
      pvb_expr = expression;
      pvb_attributes = att;
      pvb_loc = loc
    }],
    nextExp);
      pexp_attributes = [];
      pexp_loc = local;
    } -> 
    let functionAuxName = functionName^"_aux" in
    begin  
      {pexp_desc = Pexp_let 
        (label,
        (* Corpo da funcao (let rec height aux =  ...) *) 
        [
          {pvb_pat = {
            ppat_desc = Ppat_var {txt = functionAuxName; loc = varLoc};
            ppat_attributes = ppatAtt;
            ppat_loc = ppatLoc;
          };
          (* Adicionar o argumento extra (fun s -> s) *)
          pvb_expr = addArgument functionAuxName expression;
          pvb_attributes = att;
          pvb_loc = loc}
        ],
        (* Argumentos de funcao (... in tree (fun s -> s)) *)
        { pexp_desc = Pexp_apply 
          ({pexp_desc = Pexp_ident {txt = Lident functionAuxName; loc = !(default_loc)};
            pexp_attributes = [];
            pexp_loc = !(default_loc);
          }, (gatherArgsToInFunctionAuxAndAddFuncIdent expression)
        ); 
          pexp_attributes = [];
          pexp_loc = local;
        });
      pexp_attributes = []; pexp_loc = loc;
      }
    end
  | _ as e ->  eprintf "Valor: %s@." (Pprintast.string_of_expression e); assert false



(* Adicionar extensão a uma expressão => quando tratarmos de ANF adicionamos extensão CPS para que
a transformação de naive -> ANF -> CPS seja automatizada *)
let addExtensionTag extensionTag expression =
  { pstr_desc = 
    Pstr_extension (({ txt = extensionTag; loc = !(default_loc) }, 
    PStr [expression]), 
    [] (* attributes *)); 
    pstr_loc = !(default_loc)}
;;   

(* --------------------------------------------------------------------------- *)

(* Avaliar expressões *)

let rec expr_mapper mapper expr = 
  begin match expr with
    | { pexp_desc =
          Pexp_extension ({ txt = "addone"; loc }, pstr)} ->
      begin match pstr with
        | PStr [{ pstr_desc =
                    Pstr_eval (expression, _)}] -> 
          Exp.apply  (Exp.ident {txt = Lident "+"; loc=(!default_loc)})
            [(Nolabel, (expr_mapper mapper expression));
             (Nolabel, Exp.constant (Pconst_integer ("1", None)))]

        | _ -> raise (Location.Error (Location.error ~loc "Syntax error in expression mapper"))                       
      end
    | { pexp_desc =
          Pexp_extension ({ txt = "addthree"; loc }, pstr)} ->
      begin match pstr with
        | PStr [{ pstr_desc =
                    Pstr_eval (expression, _)}] -> 
                    (* Forçar a avaliar a expressão e print desta usando funcao Pprintast e Format*)
                    let expression = expr_mapper mapper expression in 
                    (* eprintf "Valor: %s@." (Pprintast.string_of_expression expression); *)
          Exp.apply  (Exp.ident {txt = Lident "+"; loc=(!default_loc)})
            [(Nolabel, (expression));
             (Nolabel, Exp.constant (Pconst_integer ("3", None)))]

        | _ -> raise (Location.Error (Location.error ~loc "Syntax error in expression mapper"))                       
      end
    | { pexp_desc =
          Pexp_extension ({ txt = "minustwo"; loc }, pstr)} ->
      begin match pstr with
        | PStr [{ pstr_desc =
                    Pstr_eval (expression, _)}] -> 
          Exp.apply  (Exp.ident {txt = Lident "-"; loc=(!default_loc)})
            [(Nolabel, (expr_mapper mapper expression));
             (Nolabel, Exp.constant (Pconst_integer ("2", None)))]

        | _ -> raise (Location.Error (Location.error ~loc "Syntax error in expression mapper"))                       
      end
    (* Delegar para default mapper de OCaml *)
    | x -> default_mapper.expr mapper x;
  end
;;
 
(* Transforma função em ANF *)
let rec changeFunctionToANF expression functionName =
    (* Retorna o corpo da função, ignorando os argumentos *)
    let bodyFunction = returnFunctionIgnoringArguments expression in
    (* Por omissão, não teremos que alterar function para match *)
    let () = changeFunctionToMatch := false in
    (* Avalia corpo da função *)
    let newBody = evaluateExpression functionName bodyFunction in
    let newExpression = 
      (if (!changeFunctionToMatch) then addMatchArgFunctionToMatchWithTransformation expression
        else expression) in
    let newFunction = changeBodyFunctionIgnoringArguments newExpression newBody in
    (* Alteração do corpo da função *)
    newFunction
    

(* Transforma função em estilo CPS (assumindo que esta está em ANF) *)
and changeFunctionToCPS functionExp expression functionName nextLetInexp =
(* Resetar variável *)
    let () = applyK := true in
    (* Criar funçãoAux in, que conterá o argumento extra (fun s -> s) *)
    let functionAuxIn = createFuncAuxInInteriorLetIn nextLetInexp functionExp in
    (* Alterar o corpo da função para introduzir a nova funçãoAux dentro deste *)
    let newBodyFunction = changeBodyFunctionIgnoringArguments expression functionAuxIn in
    (* Retornar o novo corpo da função, em estilo CPS *)
    newBodyFunction  
 
(* Avaliar structure itens *)
and eval_structure_item mapper item acc =
  match item with
  (* Avaliar rec functions *)
  | { pstr_desc = Pstr_extension (({ txt = "printStuff"; loc }, pstr), atributes); pstr_loc} ->
  begin  
    match pstr with  
    (* Avaliar se é recursiva e avaliaremos a sua expressão (pvb_expr) *)
    | (PStr [{ pstr_desc =
        Pstr_value (Recursive,
        [{
          pvb_pat = {ppat_desc = Ppat_var {txt = functionName}}; pvb_expr = expression
        }]) 
      }])  ->  
        begin   
          (* Argumentos de função: fun x -> fun y -> ... 
            A finalidade desta função é percorrer todos os argumentos da função e chegar ao
            corpo desta, ou seja, uma expressão ifthenelse, para introducao de eprintfs 
          *)
          let rec evaluateFunction = function 
            | {pexp_desc = Pexp_ifthenelse (e1,e2,Some e3);} ->
                (* Nova expressão com introducao de eprintf a referir que entrou na funcao *)
                let newExp = ifthenExp (eprintExp "Entrei em %s@." ("\""^functionName^"\"")) in 
                sequenceExpression (
                  newExp) 
                  (default_mapper.expr mapper (  
                  {
                    pexp_desc = Pexp_ifthenelse (
                       e1, (* if *)
                      (sequenceExpression (eprintExp "Valor final: %d@." "acc") e2), (* then *)
                      Some ( (* else *)
                        (* Incremento de variavel auxiliar para evitar prints repetidos 
                        de "entrei em funcao", chamada de funão de espaços para formataçao 
                        (printSpace), print de valor de N atual e avaliação de e3 *)
                        sequenceExpression 
                          incrementaVariavelAux 
                          (sequenceExpression
                              (sequenceExpression (callPrintSpace)
                              (eprintExp "@[<hv 1>@ N = %d@]@." "x"))
                            e3
                          )
                      )  
                    );
                    pexp_loc = !(default_loc);
                    pexp_attributes = [];
                  } 
                  )
                )
            (* Ainda é argumento da função. Avaliar a expressão com chamada à função funArg *)  
            | {pexp_desc = Pexp_fun (argLabel,expOption,pattern,expressao)} -> 
              {
                pexp_desc = Pexp_fun (argLabel,expOption,pattern, evaluateFunction expressao);
                pexp_attributes = [];
                pexp_loc = !(default_loc);
              }
            (* Caso em que haja erro *)  
            | _ -> raise (Location.Error (Location.error ~loc "Syntax error in expression mapper")) 
          in 
          (* Tratada o corpo da função, em newExpFuncao, incorporar esta num structure_item
             para avaliar as instrucoes seguintes (acc). É incorporado, antes da função "open
             Format" (openFormat), função auxiliar e instrução auxiliar para formatação de espaços
           *)

          (* Alterar corpo da funcao, com introdução de eprintfs, por recurso a evaluateFunction *)
          let newFunctionBody = evaluateFunction expression in 
            default_mapper.structure_item mapper 
              openFormat::printSpace::auxVariavelEspacos::( 
              { 
                pstr_desc =
                  Pstr_value 
                    (Recursive,
                      [{
                        pvb_pat = {
                        ppat_desc = Ppat_var {txt = functionName; loc = !(default_loc)};
                        ppat_loc = !(default_loc);
                        ppat_attributes = [] 
                      }; 
                      pvb_expr = newFunctionBody;
                      pvb_attributes = [];
                      pvb_loc = !(default_loc);
                      }]
                    ); 
                pstr_loc
              }
            )::acc 
        end     
    (* Caso a tag printStuf esteja associada a outra instrução que não uma função recursiva, 
    remover a tag para que o código possa ser compilado (a tag não produzira alterações no código) *)
    | PStr [otherInstruction] -> default_mapper.structure_item mapper otherInstruction :: acc
    (* Caso tenha tido a tag deverá ser do tipo PStr[...]. Este assert false é para garantir que
    o código nunca deverá chegar a este ponto *)
    | _ -> assert false 
  end
  (* Avaliar records, type x = {a:int; b:int} *)
  | { pstr_desc = Pstr_extension (({ txt = "projectRecord"; _ }, pstr), atributes); pstr_loc} -> 
  begin 
    match pstr with
    (* Verificar se tem estrutura idêntica a record (type x = {...}) de OCaml *)
    | (PStr [{ pstr_desc =
        Pstr_type (Recursive,
        [{
          ptype_name = {txt = recordName}; ptype_params = []; ptype_cstrs = []; 
          ptype_private = Public; ptype_manifest = None; ptype_kind = Ptype_record recordContentList
        }])  
      } as recordDefinition
      ])  -> 
      begin 
        (* Percorrer todas as variáveis de record *)
        let rec evaluateRecord projectionList = function
          | [] -> List.rev projectionList
          | ({pld_name = {txt = variableName}; pld_mutable = Immutable;
            pld_type = {ptyp_desc = Ptyp_constr ({txt = Lident variableType}, [])}}) :: rx ->
            begin
              (* Adicionar nova instrução de projecao ao 'acumulador' (projectionList), que 
              serão introduzidas depois do record, e avaliar próxima variável de record (rx) *)
              evaluateRecord ((recordProjection recordName variableName)::projectionList) rx
            end
          (* Só serão avaliados records com a estrutura da 2ª linha do match (implícito) *)
          | _ -> assert false        
        in 
        (* Retirar tag de record (recordDefinition) e introduzir as novas instruções (projeções), 
        por recurso à funcao evaluateRecord, depois da definição de record. Adicionar acc ao resultado
        de evaluateRecord, para que as restantes instruções do código (que podem ou não ter a ver
        com o record) possam ser avaliadas *)
        default_mapper.structure_item mapper 
          recordDefinition :: ((evaluateRecord [] recordContentList)@acc)
      end
    (* Caso a tag projectRecord esteja associada a outra instrução que não uma função recursiva, 
    remover a tag para que o código possa ser compilado (a tag não produzira alterações no código) *)
    | PStr [otherInstruction] -> default_mapper.structure_item mapper otherInstruction :: acc    
    (* Caso tenha tido a tag deverá ser do tipo PStr[...]. Este assert false é para garantir que
    o código nunca deverá chegar a este ponto *)
    | _ -> assert false
  end 
  (* Transformar em ANF, transformação intermédia de CPS, dai o nome da extensão ser CPS *)
  | { pstr_desc = Pstr_extension (({ txt = "CPS"; loc }, pstr), atributes); pstr_loc} -> 
  begin 
    match pstr with 
    (* Avaliar se é recursiva e avaliaremos a sua expressão (pvb_expr) *)
    | (PStr [{ pstr_desc =
        Pstr_value (Recursive,
        [{
          pvb_pat = {ppat_desc = Ppat_var {txt = functionName; _}; _}; pvb_expr = expression; _
        }]) 
      ; _}
      ]) -> 
      begin 
        (* Retorna o corpo da função, ignorando os argumentos *)
        let bodyFunction = returnFunctionIgnoringArguments expression in
        (* Por omissão, não teremos que alterar function para match *)
        let () = changeFunctionToMatch := false in
        (* Avalia corpo da função *)
        let newBody = evaluateExpression functionName bodyFunction in
        let newExpression = 
          (if (!changeFunctionToMatch) then addMatchArgFunctionToMatchWithTransformation expression
           else expression) in
        let newFunctionTemp = changeBodyFunctionIgnoringArguments newExpression newBody in
        (* Alteração do corpo da função *)
        let newFunction = 
          { pstr_desc =
            Pstr_value (Recursive, 
              [{
                pvb_pat = {
                  ppat_desc = Ppat_var {txt = functionName; loc = !(default_loc)};
                  ppat_attributes = [];
                  ppat_loc = !(default_loc);
                }; (* Alteração do corpo da função *)
                pvb_expr = newFunctionTemp;
                pvb_attributes = [];
                pvb_loc = !(default_loc);
              }]); 
            pstr_loc = !(default_loc)
          }
        in
        (* Estando a função em ANF, adicionar extensão para CPSFinal e reavaliar com o avaliador
        de structure item criado, não o default do OCaml *)
        let newExtensionOnNewFunction = eval_structure_item mapper 
            (addExtensionTag "CPSFinal" newFunction) acc in
        (* Retornar a nova expressão avaliada (o resultado final já terá as alterações necessárias
        para CPS, sem a extensão) *)
        newExtensionOnNewFunction  (*   default_mapper.structure_item mapper newFunction :: acc  *)
      end
    (* Caso não seja recursiva então será uma (ou mais) funções recursivas internas (let in's)
    que terão de ser transformadas para ANF e, posteriormente para CPS *)
    | (PStr [{ pstr_desc =
        Pstr_value (Nonrecursive,
        [{
          pvb_pat = {ppat_desc = Ppat_var {txt = functionName; _}; _}; pvb_expr = expression; _
        }]) 
      ; _} 
      ]) ->
        (* Ignorar/retirar argumentos da função para analisar o corpo da função *)  
        let bodyFunction = returnFunctionIgnoringArguments expression in

        (* Verificar quais as funções recursvias (let rec ... in), no corpo da função, 
        para nelaas poder aplicar transformação ANF e CPS *) 
        let rec evaluateRecFunction = function
          (* CASO 1 *)
          (* Função recursiva dentro de função com tag "CPS": let ... in let ... in ... *)
          | {pexp_desc = Pexp_let (Recursive,
              [{
                pvb_pat = {ppat_desc = Ppat_var {txt = functionName; loc = loc};
                  ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
                pvb_expr = expression; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
              }], 
              nextLetInexp); 
            pexp_attributes = pexpAtt; pexp_loc = pexpLoc
            } ->
            begin
              (* Novo body de função com transformação para ANF *)
              let pvbExpAnf = changeFunctionToANF expression functionName in
              (* Transformação temporária para bater certo com o protótipo de changeFunctionToCPS *)
              let tempAnf = {pexp_desc = Pexp_let (Recursive,
                [{
                  pvb_pat = {ppat_desc = Ppat_var {txt = functionName; loc = loc};
                    ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
                  pvb_expr = pvbExpAnf; pvb_attributes = pvbAtt; pvb_loc = pvbLoc
                }], 
                nextLetInexp); 
              pexp_attributes = pexpAtt; pexp_loc = pexpLoc
              } in

              (* Retorno de novo corpo, alterado para CPS *)
              let pvbExpCPS = changeFunctionToCPS tempAnf pvbExpAnf functionName nextLetInexp in
              {pexp_desc = Pexp_let (Recursive,
                [{
                  pvb_pat = {ppat_desc = Ppat_var {txt = functionName; loc = loc};
                    ppat_attributes = ppaAtt; ppat_loc = ppatLoc}; 
                  (* Alterar função para transformação ANF *)
                  pvb_expr = pvbExpCPS; 
                  pvb_attributes = pvbAtt; pvb_loc = pvbLoc
                }], 
                (* Recursivamente analisar a nextLetInexp (pode ser outro let in ou expressão
                genérica) *)
                evaluateRecFunction nextLetInexp); 
              pexp_attributes = pexpAtt; pexp_loc = pexpLoc}
            end
          (* E.g Let hashTable = Hashtbl.create 123 in *)
          | {pexp_desc = Pexp_let (Nonrecursive,
              pvbPattList, 
              nextLetInexp); 
            pexp_attributes = pexpAtt; pexp_loc = pexpLoc
            } ->
            {pexp_desc = Pexp_let (Nonrecursive,
              pvbPattList, 
              evaluateRecFunction nextLetInexp); 
            pexp_attributes = pexpAtt; pexp_loc = pexpLoc
            }
          (* ITE *)
          | {pexp_desc = Pexp_ifthenelse (
                ifExp, 
                thenExp, 
                Some(elseExp)); 
              pexp_attributes = att; pexp_loc = loc 
            } ->
              {pexp_desc = Pexp_ifthenelse (
                  evaluateRecFunction ifExp, 
                  evaluateRecFunction thenExp, 
                  Some(evaluateRecFunction elseExp)); 
                pexp_attributes = att; pexp_loc = loc 
              }
          (* Caso base, não aplicar transformação *)
          | _ as expression -> 
            expression 
        in

        (* Promover alterações no corpo da função *) 
        let newBody = evaluateRecFunction bodyFunction in
        (* Novo corpo da função com os argumentos reintegrados *) 
        let newFunctionTemp = changeBodyFunctionIgnoringArguments expression newBody in
        let newFunction = 
          { pstr_desc =
            Pstr_value (Nonrecursive, 
              [{
                pvb_pat = {
                  ppat_desc = Ppat_var {txt = functionName; loc = !(default_loc)};
                  ppat_attributes = [];
                  ppat_loc = !(default_loc);
                }; (* Alteração do corpo da função *)
                pvb_expr = newFunctionTemp;
                pvb_attributes = [];
                pvb_loc = !(default_loc);
              }]); 
            pstr_loc = !(default_loc)
          }
        in
        default_mapper.structure_item mapper newFunction :: acc
    (* Caso a tag projectRecord esteja associada a outra instrução que não uma função recursiva, 
    remover a tag para que o código possa ser compilado (a tag não produzira alterações no código) *)
    | PStr [otherInstruction] -> default_mapper.structure_item mapper otherInstruction :: acc    
    (* Caso tenha tido a tag deverá ser do tipo PStr[...]. Este assert false é para garantir que
    o código nunca deverá chegar a este ponto *)
    | _ -> assert false
  end
  (* Transformar em CPS *)
  | { pstr_desc = Pstr_extension (({ txt = "CPSFinal"; loc }, pstr), atributes); pstr_loc} ->
  begin
    match pstr with 
    (* Avaliar se é recursiva e avaliaremos a sua expressão (pvb_expr), retirando 
    label de recursiva, para a passar para função auxiliar interna (que conterá um argumento
    extar: função identidade *)
    | (PStr [{ pstr_desc =
        Pstr_value (Recursive,
        [{
          pvb_pat = {ppat_desc = Ppat_var {txt = functionName; _}; _}; 
          pvb_expr = expression;
          _
        }  
        ]); 
        pstr_loc = local;
      } as functionExp 
      ]) -> 
      begin
        (* Resetar variável *)
        let () = applyK := true in
       (* Criar funçãoAux in, que conterá o argumento extra (fun s -> s) *)
        let functionAuxIn = createFuncAuxIn functionExp in
        (* Alterar o corpo da função para introduzir a nova funçãoAux dentro deste *)
        let newBodyFunction = changeBodyFunctionIgnoringArguments expression functionAuxIn in
        (* Remover a tag recursiva da função inicial (preciosismo), e incorporar o novo corpo da função *)
        let newFunction = { pstr_desc =
        Pstr_value (Nonrecursive,
         [{
          pvb_pat = {
            ppat_desc = Ppat_var {txt = functionName; loc = !(default_loc)};
            ppat_attributes = [];
            ppat_loc = !(default_loc);
          };
          pvb_expr = newBodyFunction;
          pvb_attributes = [];
          pvb_loc = !(default_loc);
        }
        ]);  
        pstr_loc = local; 
      }
      in
      (* Deixar o compilador Ocaml analisar a nova função *)    
      default_mapper.structure_item mapper newFunction :: acc
    end
    (* Caso a tag projectRecord esteja associada a outra instrução que não uma função recursiva, 
    remover a tag para que o código possa ser compilado (a tag não produzira alterações no código) *)
    | PStr [otherInstruction] -> default_mapper.structure_item mapper otherInstruction :: acc    
    (* Caso tenha tido a tag deverá ser do tipo PStr[...]. Este assert false é para garantir que
    o código nunca deverá chegar a este ponto *)
    | _ -> assert false
  end   
  | _ -> default_mapper.structure_item mapper item :: acc
;;


let structure_mapper mapper structure =
  List.fold_right (eval_structure_item mapper) structure []
;;

let addone_mapper argv =
  { 
    default_mapper with
    expr = expr_mapper;
    structure = structure_mapper;
  }

let () = register "addone" addone_mapper




 