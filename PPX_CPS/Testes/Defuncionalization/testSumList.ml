open Format

let rec create_list acc = function
  | 0 -> List.rev acc
  | n -> create_list (n::acc) (n - 1)

let l = create_list [] 1_000_000

type kont = 
  | Kid
  | KConstructor_0 of int * kont

let %Defunc rec sum list =
  match list with
  | [] -> 0
  | el::rx -> el + sum rx

let () = eprintf "sum_defunc: %d@." (sum l)

let %CPS rec sum list =
  match list with
  | [] -> 0
  | el::rx -> el + sum rx

let () = eprintf "sum_CPS   : %d@." (sum l)
