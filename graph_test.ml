open Graph;;


let test_data = [|
[|None; None; Some 1; Some 1; Some 1; Some 1; None; None; None; None; None; None; None; None; None; None|];
[|None; None; Some 1; None; Some 1; None; None; Some 1; None; None; None; None; None; None; None; None|];
[|Some 1; Some 1; None; None; Some 1; Some 1; Some 1; None; None; None; None; None; None; None; None; None|];
[|Some 1; None; None; None; None; None; None; Some 1; None; None; None; None; None; None; None; None|];
[|Some 1; Some 1; Some 1; None; None; None; None; None; None; None; Some 1; None; None; None; None; None|];
[|Some 1; None; Some 1; None; None; None; None; Some 1; None; None; None; Some 1; None; None; None; None|];
[|None; None; Some 1; None; None; None; None; Some 1; None; None; None; Some 1; None; None; None; None|];
[|None; Some 1; None; Some 1; None; Some 1; Some 1; None; None; None; None; None; None; None; None; None|];
[|None; None; None; None; None; None; None; None; None; Some 1; Some 1; Some 1; None; None; Some 1; Some 1|];
[|None; None; None; None; None; None; None; None; Some 1; None; None; None; Some 1; None; Some 1; None|];
[|None; None; None; None; Some 1; None; None; None; Some 1; None; None; Some 1; Some 1; Some 1; Some 1; None|];
[|None; None; None; None; None; Some 1; Some 1; None; Some 1; None; Some 1; None; None; Some 1; None; None|];
[|None; None; None; None; None; None; None; None; None; Some 1; Some 1; None; None; None; None; None|];
[|None; None; None; None; None; None; None; None; None; None; Some 1; Some 1; None; None; None; None|];
[|None; None; None; None; None; None; None; None; Some 1; Some 1; Some 1; None; None; None; None; None|];
[|None; None; None; None; None; None; None; None; Some 1; None; None; None; None; None; None; None|];
|] in
(* let _ = Array.fold_left (fun i line ->
 * Array.fold_left (fun j elem ->
 * if i < j
 * then
 * let line = test_data.(i) in
 * line.(j) <- None; (j + 1)
 * else (j + 1)) 0 test_data; i + 1) 0 test_data in *)
let g = BaseGraph.make_graph test_data in
BaseGraph.fold_graph (fun _ node -> print_int node; print_string " ") () g; print_string " ";
BaseGraph.fold_graph (fun _ node ->
print_int node; print_string ": ";
BaseGraph.fold_neighbors (fun _ neighbor -> print_int neighbor; print_string ",") () g node;
print_string "\n") () g;; 
