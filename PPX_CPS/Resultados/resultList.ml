let length_aux len matchArg =
  let rec length_aux_aux len matchArg kFunction =
    match matchArg with
    | [] -> kFunction len
    | _::l ->
        length_aux_aux (len + 1) l
          (fun length_auxANF_0  -> kFunction length_auxANF_0)
     in
  length_aux_aux len matchArg (fun s  -> s) 
let length l = length_aux 0 l 
let cons a l = a :: l 
let hd matchArg = match matchArg with | [] -> failwith "hd" | a::_ -> a 
let tl matchArg = match matchArg with | [] -> failwith "tl" | _::l -> l 
let nth l n =
  if n < 0
  then invalid_arg "List.nth"
  else
    (let nth_aux l n =
       let rec nth_aux_aux l n kFunction kException0 =
         match l with
         | [] -> kException0 ()
         | a::l ->
             if n = 0
             then kFunction a
             else
               nth_aux_aux l (n - 1)
                 (fun nth_auxANF_0  -> kFunction nth_auxANF_0) kException0
          in
       nth_aux_aux l n (fun s  -> s) (fun ()  -> failwith "nth")  in
     nth_aux l n)
  
let nth_opt l n =
  if n < 0
  then invalid_arg "List.nth"
  else
    (let nth_aux l n =
       let rec nth_aux_aux l n kFunction =
         match l with
         | [] -> kFunction None
         | a::l ->
             if n = 0
             then kFunction (Some a)
             else
               nth_aux_aux l (n - 1)
                 (fun nth_auxANF_0  -> kFunction nth_auxANF_0)
          in
       nth_aux_aux l n (fun s  -> s)  in
     nth_aux l n)
  
let append = (@) 
let rev_append l1 l2 =
  let rec rev_append_aux l1 l2 kFunction =
    match l1 with
    | [] -> kFunction l2
    | a::l ->
        rev_append_aux l (a :: l2)
          (fun rev_appendANF_0  -> kFunction rev_appendANF_0)
     in
  rev_append_aux l1 l2 (fun s  -> s) 
let rev l = rev_append l [] 
let flatten matchArg =
  let rec flatten_aux matchArg kFunction =
    match matchArg with
    | [] -> kFunction []
    | l::r ->
        flatten_aux r (fun flattenANF_0  -> kFunction (l @ flattenANF_0))
     in
  flatten_aux matchArg (fun s  -> s) 
let concat = flatten 
let rec map f matchArg =
  let rec map_aux f matchArg kFunction =
    match matchArg with
    | [] -> kFunction []
    | a::l ->
        let r = f a  in
        map_aux f l (fun mapANF_0  -> kFunction (r :: mapANF_0))
     in
  map_aux f matchArg (fun s  -> s) 
let rec mapi i f matchArg =
  let rec mapi_aux i f matchArg kFunction =
    match matchArg with
    | [] -> kFunction []
    | a::l ->
        let r = f i a  in
        mapi_aux (i + 1) f l (fun mapiANF_0  -> kFunction (r :: mapiANF_0))
     in
  mapi_aux i f matchArg (fun s  -> s) 
let mapi f l = mapi 0 f l 
let rev_map f l =
  let rmap_f accu matchArg =
    let rec rmap_f_aux accu matchArg kFunction =
      match matchArg with
      | [] -> kFunction accu
      | a::l ->
          rmap_f_aux ((f a) :: accu) l
            (fun rmap_fANF_0  -> kFunction rmap_fANF_0)
       in
    rmap_f_aux accu matchArg (fun s  -> s)  in
  rmap_f [] l 
let iter f matchArg =
  let rec iter_aux f matchArg kFunction =
    match matchArg with
    | [] -> kFunction ()
    | a::l -> (f a; iter_aux f l (fun iterANF_0  -> kFunction iterANF_0))  in
  iter_aux f matchArg (fun s  -> s) 
let iteri i f matchArg =
  let rec iteri_aux i f matchArg kFunction =
    match matchArg with
    | [] -> kFunction ()
    | a::l ->
        (f i a;
         iteri_aux (i + 1) f l (fun iteriANF_0  -> kFunction iteriANF_0))
     in
  iteri_aux i f matchArg (fun s  -> s) 
let iteri f l = iteri 0 f l 
let fold_left f accu l =
  let rec fold_left_aux f accu l kFunction =
    match l with
    | [] -> kFunction accu
    | a::l ->
        fold_left_aux f (f accu a) l
          (fun fold_leftANF_0  -> kFunction fold_leftANF_0)
     in
  fold_left_aux f accu l (fun s  -> s) 
let fold_right f l accu =
  let rec fold_right_aux f l accu kFunction =
    match l with
    | [] -> kFunction accu
    | a::l ->
        fold_right_aux f l accu
          (fun fold_rightANF_0  -> kFunction (f a fold_rightANF_0))
     in
  fold_right_aux f l accu (fun s  -> s) 
let rec map2 f l1 l2 =
  let rec map2_aux f l1 l2 kFunction kException0 =
    match (l1, l2) with
    | ([],[]) -> kFunction []
    | (a1::l1,a2::l2) ->
        let r = f a1 a2  in
        map2_aux f l1 l2 (fun map2ANF_0  -> kFunction (r :: map2ANF_0))
          kException0
    | (_,_) -> kException0 ()  in
  map2_aux f l1 l2 (fun s  -> s) (fun ()  -> invalid_arg "List.map2") 
let rev_map2 f l1 l2 =
  let rmap2_f accu l1 l2 =
    let rec rmap2_f_aux accu l1 l2 kFunction kException0 =
      match (l1, l2) with
      | ([],[]) -> kFunction accu
      | (a1::l1,a2::l2) ->
          rmap2_f_aux ((f a1 a2) :: accu) l1 l2
            (fun rmap2_fANF_0  -> kFunction rmap2_fANF_0) kException0
      | (_,_) -> kException0 ()  in
    rmap2_f_aux accu l1 l2 (fun s  -> s)
      (fun ()  -> invalid_arg "List.rev_map2")
     in
  rmap2_f [] l1 l2 
let iter2 f l1 l2 =
  let rec iter2_aux f l1 l2 kFunction kException0 =
    match (l1, l2) with
    | ([],[]) -> kFunction ()
    | (a1::l1,a2::l2) ->
        (f a1 a2;
         iter2_aux f l1 l2 (fun iter2ANF_0  -> kFunction iter2ANF_0)
           kException0)
    | (_,_) -> kException0 ()  in
  iter2_aux f l1 l2 (fun s  -> s) (fun ()  -> invalid_arg "List.iter2") 
let fold_left2 f accu l1 l2 =
  let rec fold_left2_aux f accu l1 l2 kFunction kException0 =
    match (l1, l2) with
    | ([],[]) -> kFunction accu
    | (a1::l1,a2::l2) ->
        fold_left2_aux f (f accu a1 a2) l1 l2
          (fun fold_left2ANF_0  -> kFunction fold_left2ANF_0) kException0
    | (_,_) -> kException0 ()  in
  fold_left2_aux f accu l1 l2 (fun s  -> s)
    (fun ()  -> invalid_arg "List.fold_left2")
  
let fold_right2 f l1 l2 accu =
  let rec fold_right2_aux f l1 l2 accu kFunction kException0 =
    match (l1, l2) with
    | ([],[]) -> kFunction accu
    | (a1::l1,a2::l2) ->
        fold_right2_aux f l1 l2 accu
          (fun fold_right2ANF_0  -> kFunction (f a1 a2 fold_right2ANF_0))
          kException0
    | (_,_) -> kException0 ()  in
  fold_right2_aux f l1 l2 accu (fun s  -> s)
    (fun ()  -> invalid_arg "List.fold_right2")
  
let for_all p matchArg =
  let rec for_all_aux p matchArg kFunction =
    match matchArg with
    | [] -> kFunction true
    | a::l ->
        for_all_aux p l
          (fun for_allANF_0  -> kFunction ((p a) && for_allANF_0))
     in
  for_all_aux p matchArg (fun s  -> s) 
let exists p matchArg =
  let rec exists_aux p matchArg kFunction =
    match matchArg with
    | [] -> kFunction false
    | a::l ->
        exists_aux p l (fun existsANF_0  -> kFunction ((p a) || existsANF_0))
     in
  exists_aux p matchArg (fun s  -> s) 
let for_all2 p l1 l2 =
  let rec for_all2_aux p l1 l2 kFunction kException0 =
    match (l1, l2) with
    | ([],[]) -> kFunction true
    | (a1::l1,a2::l2) ->
        for_all2_aux p l1 l2
          (fun for_all2ANF_0  -> kFunction ((p a1 a2) && for_all2ANF_0))
          kException0
    | (_,_) -> kException0 ()  in
  for_all2_aux p l1 l2 (fun s  -> s) (fun ()  -> invalid_arg "List.for_all2") 
let exists2 p l1 l2 =
  let rec exists2_aux p l1 l2 kFunction kException0 =
    match (l1, l2) with
    | ([],[]) -> kFunction false
    | (a1::l1,a2::l2) ->
        exists2_aux p l1 l2
          (fun exists2ANF_0  -> kFunction ((p a1 a2) || exists2ANF_0))
          kException0
    | (_,_) -> kException0 ()  in
  exists2_aux p l1 l2 (fun s  -> s) (fun ()  -> invalid_arg "List.exists2") 
let mem x matchArg =
  let rec mem_aux x matchArg kFunction =
    match matchArg with
    | [] -> kFunction false
    | a::l ->
        mem_aux x l
          (fun memANF_0  -> kFunction (((compare a x) = 0) || memANF_0))
     in
  mem_aux x matchArg (fun s  -> s) 
let memq x matchArg =
  let rec memq_aux x matchArg kFunction =
    match matchArg with
    | [] -> kFunction false
    | a::l ->
        memq_aux x l (fun memqANF_0  -> kFunction ((a == x) || memqANF_0))
     in
  memq_aux x matchArg (fun s  -> s) 
let assoc x matchArg =
  let rec assoc_aux x matchArg kFunction kException0 =
    match matchArg with
    | [] -> kException0 ()
    | (a,b)::l ->
        if (compare a x) = 0
        then kFunction b
        else
          assoc_aux x l (fun assocANF_0  -> kFunction assocANF_0) kException0
     in
  assoc_aux x matchArg (fun s  -> s) (fun ()  -> raise Not_found) 
let assoc_opt x matchArg =
  let rec assoc_opt_aux x matchArg kFunction =
    match matchArg with
    | [] -> kFunction None
    | (a,b)::l ->
        if (compare a x) = 0
        then kFunction (Some b)
        else
          assoc_opt_aux x l (fun assoc_optANF_0  -> kFunction assoc_optANF_0)
     in
  assoc_opt_aux x matchArg (fun s  -> s) 
let assq x matchArg =
  let rec assq_aux x matchArg kFunction kException0 =
    match matchArg with
    | [] -> kException0 ()
    | (a,b)::l ->
        if a == x
        then kFunction b
        else assq_aux x l (fun assqANF_0  -> kFunction assqANF_0) kException0
     in
  assq_aux x matchArg (fun s  -> s) (fun ()  -> raise Not_found) 
let assq_opt x matchArg =
  let rec assq_opt_aux x matchArg kFunction =
    match matchArg with
    | [] -> kFunction None
    | (a,b)::l ->
        if a == x
        then kFunction (Some b)
        else assq_opt_aux x l (fun assq_optANF_0  -> kFunction assq_optANF_0)
     in
  assq_opt_aux x matchArg (fun s  -> s) 
let mem_assoc x matchArg =
  let rec mem_assoc_aux x matchArg kFunction =
    match matchArg with
    | [] -> kFunction false
    | (a,_)::l ->
        mem_assoc_aux x l
          (fun mem_assocANF_0  ->
             kFunction (((compare a x) = 0) || mem_assocANF_0))
     in
  mem_assoc_aux x matchArg (fun s  -> s) 
let mem_assq x matchArg =
  let rec mem_assq_aux x matchArg kFunction =
    match matchArg with
    | [] -> kFunction false
    | (a,_)::l ->
        mem_assq_aux x l
          (fun mem_assqANF_0  -> kFunction ((a == x) || mem_assqANF_0))
     in
  mem_assq_aux x matchArg (fun s  -> s) 
let remove_assoc x matchArg =
  let rec remove_assoc_aux x matchArg kFunction =
    match matchArg with
    | [] -> kFunction []
    | ((a,_) as pair)::l ->
        if (compare a x) = 0
        then kFunction l
        else
          remove_assoc_aux x l
            (fun remove_assocANF_0  -> kFunction (pair :: remove_assocANF_0))
     in
  remove_assoc_aux x matchArg (fun s  -> s) 
let remove_assq x matchArg =
  let rec remove_assq_aux x matchArg kFunction =
    match matchArg with
    | [] -> kFunction []
    | ((a,_) as pair)::l ->
        if a == x
        then kFunction l
        else
          remove_assq_aux x l
            (fun remove_assqANF_0  -> kFunction (pair :: remove_assqANF_0))
     in
  remove_assq_aux x matchArg (fun s  -> s) 
let find p matchArg =
  let rec find_aux p matchArg kFunction kException0 =
    match matchArg with
    | [] -> kException0 ()
    | x::l ->
        if p x
        then kFunction x
        else find_aux p l (fun findANF_0  -> kFunction findANF_0) kException0
     in
  find_aux p matchArg (fun s  -> s) (fun ()  -> raise Not_found) 
let find_opt p matchArg =
  let rec find_opt_aux p matchArg kFunction =
    match matchArg with
    | [] -> kFunction None
    | x::l ->
        if p x
        then kFunction (Some x)
        else find_opt_aux p l (fun find_optANF_0  -> kFunction find_optANF_0)
     in
  find_opt_aux p matchArg (fun s  -> s) 
let find_all p =
  let find accu matchArg =
    let rec find_aux accu matchArg kFunction =
      match matchArg with
      | [] -> kFunction (rev accu)
      | x::l ->
          if p x
          then find_aux (x :: accu) l (fun findANF_0  -> kFunction findANF_0)
          else find_aux accu l (fun findANF_0  -> kFunction findANF_0)
       in
    find_aux accu matchArg (fun s  -> s)  in
  find [] 
let filter = find_all 
let partition p l =
  let part yes no matchArg =
    let rec part_aux yes no matchArg kFunction =
      match matchArg with
      | [] -> kFunction ((rev yes), (rev no))
      | x::l ->
          if p x
          then
            part_aux (x :: yes) no l (fun partANF_0  -> kFunction partANF_0)
          else
            part_aux yes (x :: no) l (fun partANF_0  -> kFunction partANF_0)
       in
    part_aux yes no matchArg (fun s  -> s)  in
  part [] [] l 
let rec split matchArg =
  let rec split_aux matchArg kFunction =
    match matchArg with
    | [] -> kFunction ([], [])
    | (x,y)::l -> let (rx,ry) = split l  in kFunction ((x :: rx), (y :: ry))
     in
  split_aux matchArg (fun s  -> s) 
let combine l1 l2 =
  let rec combine_aux l1 l2 kFunction kException0 =
    match (l1, l2) with
    | ([],[]) -> kFunction []
    | (a1::l1,a2::l2) ->
        combine_aux l1 l2
          (fun combineANF_0  -> kFunction ((a1, a2) :: combineANF_0))
          kException0
    | (_,_) -> kException0 ()  in
  combine_aux l1 l2 (fun s  -> s) (fun ()  -> invalid_arg "List.combine") 
[@@@ocaml.text " sorting "]
let merge cmp l1 l2 =
  let rec merge_aux cmp l1 l2 kFunction =
    match (l1, l2) with
    | ([],l2) -> kFunction l2
    | (l1,[]) -> kFunction l1
    | (h1::t1,h2::t2) ->
        if (cmp h1 h2) <= 0
        then
          merge_aux cmp t1 l2
            (fun mergeANF_0  -> kFunction (h1 :: mergeANF_0))
        else
          merge_aux cmp l1 t2
            (fun mergeANF_0  -> kFunction (h2 :: mergeANF_0))
     in
  merge_aux cmp l1 l2 (fun s  -> s) 
let chop k l =
  let rec chop_aux k l kFunction kException0 =
    if k = 0
    then kFunction l
    else
      (match l with
       | _::t ->
           chop_aux (k - 1) t (fun chopANF_0  -> kFunction chopANF_0)
             kException0
       | _ -> kException0 ())
     in
  chop_aux k l (fun s  -> s) (fun ()  -> assert false) 
let stable_sort cmp l =
  let rev_merge l1 l2 accu =
    let rec rev_merge_aux l1 l2 accu kFunction =
      match (l1, l2) with
      | ([],l2) -> kFunction (rev_append l2 accu)
      | (l1,[]) -> kFunction (rev_append l1 accu)
      | (h1::t1,h2::t2) ->
          if (cmp h1 h2) <= 0
          then
            rev_merge_aux t1 l2 (h1 :: accu)
              (fun rev_mergeANF_0  -> kFunction rev_mergeANF_0)
          else
            rev_merge_aux l1 t2 (h2 :: accu)
              (fun rev_mergeANF_0  -> kFunction rev_mergeANF_0)
       in
    rev_merge_aux l1 l2 accu (fun s  -> s)  in
  let rev_merge_rev l1 l2 accu =
    let rec rev_merge_rev_aux l1 l2 accu kFunction =
      match (l1, l2) with
      | ([],l2) -> kFunction (rev_append l2 accu)
      | (l1,[]) -> kFunction (rev_append l1 accu)
      | (h1::t1,h2::t2) ->
          if (cmp h1 h2) > 0
          then
            rev_merge_rev_aux t1 l2 (h1 :: accu)
              (fun rev_merge_revANF_0  -> kFunction rev_merge_revANF_0)
          else
            rev_merge_rev_aux l1 t2 (h2 :: accu)
              (fun rev_merge_revANF_0  -> kFunction rev_merge_revANF_0)
       in
    rev_merge_rev_aux l1 l2 accu (fun s  -> s)  in
  let rec sort n l =
    let rec sort_aux n l kFunction =
      match (n, l) with
      | (2,x1::x2::_) ->
          if (cmp x1 x2) <= 0 then kFunction [x1; x2] else kFunction [x2; x1]
      | (3,x1::x2::x3::_) ->
          if (cmp x1 x2) <= 0
          then
            (if (cmp x2 x3) <= 0
             then kFunction [x1; x2; x3]
             else
               if (cmp x1 x3) <= 0
               then kFunction [x1; x3; x2]
               else kFunction [x3; x1; x2])
          else
            if (cmp x1 x3) <= 0
            then kFunction [x2; x1; x3]
            else
              if (cmp x2 x3) <= 0
              then kFunction [x2; x3; x1]
              else kFunction [x3; x2; x1]
      | (n,l) ->
          let n1 = n asr 1  in
          let n2 = n - n1  in
          let l2 = chop n1 l  in
          let s1 = rev_sort n1 l  in
          let s2 = rev_sort n2 l2  in kFunction (rev_merge_rev s1 s2 [])
       in
    sort_aux n l (fun s  -> s)
  
  and rev_sort n l =
    let rec rev_sort_aux n l kFunction =
      match (n, l) with
      | (2,x1::x2::_) ->
          if (cmp x1 x2) > 0 then kFunction [x1; x2] else kFunction [x2; x1]
      | (3,x1::x2::x3::_) ->
          if (cmp x1 x2) > 0
          then
            (if (cmp x2 x3) > 0
             then kFunction [x1; x2; x3]
             else
               if (cmp x1 x3) > 0
               then kFunction [x1; x3; x2]
               else kFunction [x3; x1; x2])
          else
            if (cmp x1 x3) > 0
            then kFunction [x2; x1; x3]
            else
              if (cmp x2 x3) > 0
              then kFunction [x2; x3; x1]
              else kFunction [x3; x2; x1]
      | (n,l) ->
          let n1 = n asr 1  in
          let n2 = n - n1  in
          let l2 = chop n1 l  in
          let s1 = sort n1 l  in
          let s2 = sort n2 l2  in kFunction (rev_merge s1 s2 [])
       in
    rev_sort_aux n l (fun s  -> s)
   in let len = length l  in if len < 2 then l else sort len l 
let sort = stable_sort 
let fast_sort = stable_sort 
[@@@ocaml.text " sorting + removing duplicates "]
let sort_uniq cmp l =
  let rec rev_merge l1 l2 accu =
    let rec rev_merge_aux l1 l2 accu kFunction =
      match (l1, l2) with
      | ([],l2) -> kFunction (rev_append l2 accu)
      | (l1,[]) -> kFunction (rev_append l1 accu)
      | (h1::t1,h2::t2) ->
          let c = cmp h1 h2  in
          if c = 0
          then
            rev_merge_aux t1 t2 (h1 :: accu)
              (fun rev_mergeANF_0  -> kFunction rev_mergeANF_0)
          else
            if c < 0
            then
              rev_merge_aux t1 l2 (h1 :: accu)
                (fun rev_mergeANF_0  -> kFunction rev_mergeANF_0)
            else
              rev_merge_aux l1 t2 (h2 :: accu)
                (fun rev_mergeANF_0  -> kFunction rev_mergeANF_0)
       in
    rev_merge_aux l1 l2 accu (fun s  -> s)  in
  let rec rev_merge_rev l1 l2 accu =
    let rec rev_merge_rev_aux l1 l2 accu kFunction =
      match (l1, l2) with
      | ([],l2) -> kFunction (rev_append l2 accu)
      | (l1,[]) -> kFunction (rev_append l1 accu)
      | (h1::t1,h2::t2) ->
          let c = cmp h1 h2  in
          if c = 0
          then
            rev_merge_rev_aux t1 t2 (h1 :: accu)
              (fun rev_merge_revANF_0  -> kFunction rev_merge_revANF_0)
          else
            if c > 0
            then
              rev_merge_rev_aux t1 l2 (h1 :: accu)
                (fun rev_merge_revANF_0  -> kFunction rev_merge_revANF_0)
            else
              rev_merge_rev_aux l1 t2 (h2 :: accu)
                (fun rev_merge_revANF_0  -> kFunction rev_merge_revANF_0)
       in
    rev_merge_rev_aux l1 l2 accu (fun s  -> s)  in
  let rec sort n l =
    let rec sort_aux n l kFunction =
      match (n, l) with
      | (2,x1::x2::_) ->
          let c = cmp x1 x2  in
          if c = 0
          then kFunction [x1]
          else if c < 0 then kFunction [x1; x2] else kFunction [x2; x1]
      | (3,x1::x2::x3::_) ->
          let c = cmp x1 x2  in
          if c = 0
          then
            let c = cmp x2 x3  in
            (if c = 0
             then kFunction [x2]
             else if c < 0 then kFunction [x2; x3] else kFunction [x3; x2])
          else
            if c < 0
            then
              (let c = cmp x2 x3  in
               if c = 0
               then kFunction [x1; x2]
               else
                 if c < 0
                 then kFunction [x1; x2; x3]
                 else
                   (let c = cmp x1 x3  in
                    if c = 0
                    then kFunction [x1; x2]
                    else
                      if c < 0
                      then kFunction [x1; x3; x2]
                      else kFunction [x3; x1; x2]))
            else
              (let c = cmp x1 x3  in
               if c = 0
               then kFunction [x2; x1]
               else
                 if c < 0
                 then kFunction [x2; x1; x3]
                 else
                   (let c = cmp x2 x3  in
                    if c = 0
                    then kFunction [x2; x1]
                    else
                      if c < 0
                      then kFunction [x2; x3; x1]
                      else kFunction [x3; x2; x1]))
      | (n,l) ->
          let n1 = n asr 1  in
          let n2 = n - n1  in
          let l2 = chop n1 l  in
          let s1 = rev_sort n1 l  in
          let s2 = rev_sort n2 l2  in kFunction (rev_merge_rev s1 s2 [])
       in
    sort_aux n l (fun s  -> s)
  
  and rev_sort n l =
    let rec rev_sort_aux n l kFunction =
      match (n, l) with
      | (2,x1::x2::_) ->
          let c = cmp x1 x2  in
          if c = 0
          then kFunction [x1]
          else if c > 0 then kFunction [x1; x2] else kFunction [x2; x1]
      | (3,x1::x2::x3::_) ->
          let c = cmp x1 x2  in
          if c = 0
          then
            let c = cmp x2 x3  in
            (if c = 0
             then kFunction [x2]
             else if c > 0 then kFunction [x2; x3] else kFunction [x3; x2])
          else
            if c > 0
            then
              (let c = cmp x2 x3  in
               if c = 0
               then kFunction [x1; x2]
               else
                 if c > 0
                 then kFunction [x1; x2; x3]
                 else
                   (let c = cmp x1 x3  in
                    if c = 0
                    then kFunction [x1; x2]
                    else
                      if c > 0
                      then kFunction [x1; x3; x2]
                      else kFunction [x3; x1; x2]))
            else
              (let c = cmp x1 x3  in
               if c = 0
               then kFunction [x2; x1]
               else
                 if c > 0
                 then kFunction [x2; x1; x3]
                 else
                   (let c = cmp x2 x3  in
                    if c = 0
                    then kFunction [x2; x1]
                    else
                      if c > 0
                      then kFunction [x2; x3; x1]
                      else kFunction [x3; x2; x1]))
      | (n,l) ->
          let n1 = n asr 1  in
          let n2 = n - n1  in
          let l2 = chop n1 l  in
          let s1 = sort n1 l  in
          let s2 = sort n2 l2  in kFunction (rev_merge s1 s2 [])
       in
    rev_sort_aux n l (fun s  -> s)
   in let len = length l  in if len < 2 then l else sort len l 
let compare_lengths l1 l2 =
  let rec compare_lengths_aux l1 l2 kFunction =
    match (l1, l2) with
    | ([],[]) -> kFunction 0
    | ([],_) -> kFunction (-1)
    | (_,[]) -> kFunction 1
    | (_::l1,_::l2) ->
        compare_lengths_aux l1 l2
          (fun compare_lengthsANF_0  -> kFunction compare_lengthsANF_0)
     in
  compare_lengths_aux l1 l2 (fun s  -> s) 
let compare_length_with l n = 
  let rec compare_length_with_aux l n kFunction =
    match (l, n) with
    | ([],0) -> kFunction 0
    | ([],_) -> if n > 0 then kFunction (-1) else kFunction 1
    | (_,0) -> kFunction 1
    | (_::l,n) ->
        compare_length_with_aux l (n - 1)
          (fun compare_length_withANF_0  ->
             kFunction compare_length_withANF_0)
     in
  compare_length_with_aux l n (fun s  -> s) 
