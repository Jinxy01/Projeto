open Array
let merge order l1 l2 =
  let rec merge_aux order l1 l2 kFunction =
    match l1 with
    | [] -> kFunction l2
    | h1::t1 ->
        (match l2 with
         | [] -> kFunction l1
         | h2::t2 ->
             if order h1 h2
             then
               merge_aux order t1 l2
                 (fun mergeANF_0  -> kFunction (h1 :: mergeANF_0))
             else
               merge_aux order l1 t2
                 (fun mergeANF_0  -> kFunction (h2 :: mergeANF_0)))
     in
  merge_aux order l1 l2 (fun s  -> s) 
let list order l =
  let initlist matchArg =
    let rec initlist_aux matchArg kFunction =
      match matchArg with
      | [] -> kFunction []
      | e::[] -> kFunction [[e]]
      | e1::e2::rest ->
          initlist_aux rest
            (fun initlistANF_0  ->
               kFunction ((if order e1 e2 then [e1; e2] else [e2; e1]) ::
                 initlistANF_0))
       in
    initlist_aux matchArg (fun s  -> s)  in
  let merge2 matchArg =
    let rec merge2_aux matchArg kFunction =
      match matchArg with
      | l1::l2::rest ->
          merge2_aux rest
            (fun merge2ANF_0  ->
               kFunction ((merge order l1 l2) :: merge2ANF_0))
      | x -> kFunction x  in
    merge2_aux matchArg (fun s  -> s)  in
  let mergeall matchArg =
    let rec mergeall_aux matchArg kFunction =
      match matchArg with
      | [] -> kFunction []
      | l::[] -> kFunction l
      | llist ->
          mergeall_aux (merge2 llist)
            (fun mergeallANF_0  -> kFunction mergeallANF_0)
       in
    mergeall_aux matchArg (fun s  -> s)  in
  mergeall (initlist l) 
let swap arr i j =
  let tmp = unsafe_get arr i  in
  unsafe_set arr i (unsafe_get arr j); unsafe_set arr j tmp 
let array cmp arr =
  let rec qsort lo hi =
    let rec qsort_aux lo hi kFunction =
      if (hi - lo) >= 6
      then
        let mid = (lo + hi) lsr 1  in
        (if cmp (unsafe_get arr mid) (unsafe_get arr lo)
         then kFunction (swap arr mid lo);
         if cmp (unsafe_get arr hi) (unsafe_get arr mid)
         then
           (kFunction (swap arr mid hi);
            if cmp (unsafe_get arr mid) (unsafe_get arr lo)
            then kFunction (swap arr mid lo));
         (let pivot = unsafe_get arr mid  in
          let i = ref (lo + 1)
          
          and j =
            let rec j_aux kFunction = kFunction (ref (hi - 1))  in
            j_aux (fun s  -> s)
           in
          if
            (not (cmp pivot (unsafe_get arr hi))) ||
              (not (cmp (unsafe_get arr lo) pivot))
          then raise (Invalid_argument "Sort.array");
          while (!i) < (!j) do
            (while not (cmp pivot (unsafe_get arr (!i))) do incr i done;
             while not (cmp (unsafe_get arr (!j)) pivot) do decr j done;
             if (!i) < (!j) then swap arr (!i) (!j);
             incr i;
             decr j)
            done;
          if ((!j) - lo) <= (hi - (!i))
          then
            (qsort_aux lo (!j) (fun qsortANF_0  -> kFunction qsortANF_0);
             qsort_aux (!i) hi (fun qsortANF_0  -> kFunction qsortANF_0))
          else
            (qsort_aux (!i) hi (fun qsortANF_0  -> kFunction qsortANF_0);
             qsort_aux lo (!j) (fun qsortANF_0  -> kFunction qsortANF_0))))
       in
    qsort_aux lo hi (fun s  -> s)  in
  qsort 0 ((Array.length arr) - 1);
  for i = 1 to (Array.length arr) - 1 do
    (let val_i = unsafe_get arr i  in
     if not (cmp (unsafe_get arr (i - 1)) val_i)
     then
       (unsafe_set arr i (unsafe_get arr (i - 1));
        (let j = ref (i - 1)  in
         while ((!j) >= 1) && (not (cmp (unsafe_get arr ((!j) - 1)) val_i))
           do (unsafe_set arr (!j) (unsafe_get arr ((!j) - 1)); decr j) done;
         unsafe_set arr (!j) val_i)))
  done 
