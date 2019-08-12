Testes/test.ml
==>
[{pstr_desc =
   Pstr_type (Recursive,
    [{ptype_name = {txt = "t"}; ptype_params = []; ptype_cstrs = [];
      ptype_kind =
       Ptype_variant
        [{pcd_name = {txt = "E"}; pcd_args = Pcstr_tuple []; pcd_res = None};
         {pcd_name = {txt = "N"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "t"}, [])};
             {ptyp_desc = Ptyp_constr ({txt = Lident "t"}, [])}];
          pcd_res = None}];
      ptype_private = Public; ptype_manifest = None}])};
 {pstr_desc =
   Pstr_value (Recursive,
    [{pvb_pat = {ppat_desc = Ppat_var {txt = "create_list"}};
      pvb_expr =
       {pexp_desc =
         Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt = "acc"}},
          {pexp_desc =
            Pexp_function
             [{pc_lhs =
                {ppat_desc = Ppat_constant (Pconst_integer ("0", None))};
               pc_guard = None;
               pc_rhs =
                {pexp_desc =
                  Pexp_apply
                   ({pexp_desc =
                      Pexp_ident {txt = Ldot (Lident "List", "rev")}},
                   [(Nolabel, {pexp_desc = Pexp_ident {txt = Lident "acc"}})])}};
              {pc_lhs = {ppat_desc = Ppat_var {txt = "n"}}; pc_guard = None;
               pc_rhs =
                {pexp_desc =
                  Pexp_apply
                   ({pexp_desc = Pexp_ident {txt = Lident "create_list"}},
                   [(Nolabel,
                     {pexp_desc =
                       Pexp_construct ({txt = Lident "::"},
                        Some
                         {pexp_desc =
                           Pexp_tuple
                            [{pexp_desc = Pexp_ident {txt = Lident "n"}};
                             {pexp_desc = Pexp_ident {txt = Lident "acc"}}]})});
                    (Nolabel,
                     {pexp_desc =
                       Pexp_apply
                        ({pexp_desc = Pexp_ident {txt = Lident "-"}},
                        [(Nolabel,
                          {pexp_desc = Pexp_ident {txt = Lident "n"}});
                         (Nolabel,
                          {pexp_desc =
                            Pexp_constant (Pconst_integer ("1", None))})])})])}}]})}}])};
 {pstr_desc =
   Pstr_value (Nonrecursive,
    [{pvb_pat = {ppat_desc = Ppat_var {txt = "l"}};
      pvb_expr =
       {pexp_desc =
         Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "create_list"}},
          [(Nolabel,
            {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)});
           (Nolabel,
            {pexp_desc = Pexp_constant (Pconst_integer ("1_000_000", None))})])}}])};
 {pstr_desc =
   Pstr_type (Recursive,
    [{ptype_name = {txt = "kont"}; ptype_params = []; ptype_cstrs = [];
      ptype_kind =
       Ptype_variant
        [{pcd_name = {txt = "Kid"}; pcd_args = Pcstr_tuple [];
          pcd_res = None};
         {pcd_name = {txt = "KConstructor_0"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "int"}, [])};
             {ptyp_desc = Ptyp_constr ({txt = Lident "kont"}, [])}];
          pcd_res = None}];
      ptype_private = Public; ptype_manifest = None}])};
 {pstr_desc =
   Pstr_extension
    (({txt = "CPS"},
      PStr
       [{pstr_desc =
          Pstr_value (Recursive,
           [{pvb_pat = {ppat_desc = Ppat_var {txt = "sum"}};
             pvb_expr =
              {pexp_desc =
                Pexp_function
                 [{pc_lhs =
                    {ppat_desc = Ppat_construct ({txt = Lident "[]"}, None)};
                   pc_guard = None;
                   pc_rhs =
                    {pexp_desc = Pexp_constant (Pconst_integer ("0", None))}};
                  {pc_lhs =
                    {ppat_desc =
                      Ppat_construct ({txt = Lident "::"},
                       Some
                        {ppat_desc =
                          Ppat_tuple
                           [{ppat_desc = Ppat_var {txt = "el"}};
                            {ppat_desc = Ppat_var {txt = "rx"}}]})};
                   pc_guard = None;
                   pc_rhs =
                    {pexp_desc =
                      Pexp_apply
                       ({pexp_desc = Pexp_ident {txt = Lident "+"}},
                       [(Nolabel,
                         {pexp_desc = Pexp_ident {txt = Lident "el"}});
                        (Nolabel,
                         {pexp_desc =
                           Pexp_apply
                            ({pexp_desc = Pexp_ident {txt = Lident "sum"}},
                            [(Nolabel,
                              {pexp_desc = Pexp_ident {txt = Lident "rx"}})])})])}}]}}])}]),
    ...)};
 {pstr_desc =
   Pstr_value (Nonrecursive,
    [{pvb_pat = {ppat_desc = Ppat_var {txt = "time"}};
      pvb_expr =
       {pexp_desc =
         Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt = "f"}},
          {pexp_desc =
            Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt = "x"}},
             {pexp_desc =
               Pexp_fun (Nolabel, None,
                {ppat_desc = Ppat_var {txt = "name"}},
                {pexp_desc =
                  Pexp_let (Nonrecursive,
                   [{pvb_pat = {ppat_desc = Ppat_var {txt = "t"}};
                     pvb_expr =
                      {pexp_desc =
                        Pexp_apply
                         ({pexp_desc =
                            Pexp_ident {txt = Ldot (Lident "Sys", "time")}},
                         [(Nolabel,
                           {pexp_desc =
                             Pexp_construct ({txt = Lident "()"}, None)})])}}],
                   {pexp_desc =
                     Pexp_let (Nonrecursive,
                      [{pvb_pat = {ppat_desc = Ppat_any};
                        pvb_expr =
                         {pexp_desc =
                           Pexp_apply
                            ({pexp_desc = Pexp_ident {txt = Lident "f"}},
                            [(Nolabel,
                              {pexp_desc = Pexp_ident {txt = Lident "x"}})])}}],
                      {pexp_desc =
                        Pexp_apply
                         ({pexp_desc =
                            Pexp_ident
                             {txt = Ldot (Lident "Format", "eprintf")}},
                         [(Nolabel,
                           {pexp_desc =
                             Pexp_constant
                              (Pconst_string
                                ("Execution time of %s: @. %fs @.", None))});
                          (Nolabel,
                           {pexp_desc = Pexp_ident {txt = Lident "name"}});
                          (Nolabel,
                           {pexp_desc =
                             Pexp_apply
                              ({pexp_desc = Pexp_ident {txt = Lident "-."}},
                              [(Nolabel,
                                {pexp_desc =
                                  Pexp_apply
                                   ({pexp_desc =
                                      Pexp_ident
                                       {txt = Ldot (Lident "Sys", "time")}},
                                   [(Nolabel,
                                     {pexp_desc =
                                       Pexp_construct ({txt = Lident "()"},
                                        None)})])});
                               (Nolabel,
                                {pexp_desc = Pexp_ident {txt = Lident "t"}})])})])})})})})})}}])};
 {pstr_desc =
   Pstr_value (Nonrecursive,
    [{pvb_pat = {ppat_desc = Ppat_construct ({txt = Lident "()"}, None)};
      pvb_expr =
       {pexp_desc =
         Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "time"}},
          [(Nolabel, {pexp_desc = Pexp_ident {txt = Lident "sum"}});
           (Nolabel, {pexp_desc = Pexp_ident {txt = Lident "l"}});
           (Nolabel,
            {pexp_desc = Pexp_constant (Pconst_string ("sum (l)", None))})])}}])};
 {pstr_desc =
   Pstr_value (Nonrecursive,
    [{pvb_pat = {ppat_desc = Ppat_var {txt = "mem_used"}};
      pvb_expr =
       {pexp_desc =
         Pexp_apply
          ({pexp_desc =
             Pexp_ident {txt = Ldot (Lident "Gc", "allocated_bytes")}},
          [(Nolabel,
            {pexp_desc = Pexp_construct ({txt = Lident "()"}, None)})])}}])};
 {pstr_desc =
   Pstr_value (Nonrecursive,
    [{pvb_pat = {ppat_desc = Ppat_construct ({txt = Lident "()"}, None)};
      pvb_expr =
       {pexp_desc =
         Pexp_apply
          ({pexp_desc = Pexp_ident {txt = Ldot (Lident "Format", "eprintf")}},
          [(Nolabel,
            {pexp_desc =
              Pexp_constant (Pconst_string ("Valor = %f MB@.", None))});
           (Nolabel,
            {pexp_desc =
              Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "/."}},
               [(Nolabel,
                 {pexp_desc =
                   Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "/."}},
                    [(Nolabel,
                      {pexp_desc = Pexp_ident {txt = Lident "mem_used"}});
                     (Nolabel,
                      {pexp_desc =
                        Pexp_constant (Pconst_float ("1024.0", None))})])});
                (Nolabel,
                 {pexp_desc = Pexp_constant (Pconst_float ("1024.0", None))})])})])}}])}]
=========
