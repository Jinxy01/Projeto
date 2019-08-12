open Format

type kont = 
  | Kid
  | KConstructor_0 of int * kont

let %Defunc rec fat n =
  if n = 0 then 1
  else n * fat (n - 1) 
let n = 5 

let () = eprintf "fatorial_defunc: %d@." (fat n)

let %CPS rec fat n =
  if n = 0 then 1
  else n * fat (n - 1) 

let () = eprintf "fatorial_CPS   : %d@." (fat n)