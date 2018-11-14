open Graph
open Utils
open Data

;;
let graph = BaseGraph.make_graph test_data_16 in
let _ = print_graph graph in
let sub1 = BaseGraph.extract_graph graph 8 in
let sub2 = BaseGraph.extract_graph graph 15 in
let _ = print_graph sub1 in
let _ = print_graph sub2 in
let _ = BaseGraph.merge_node_crossgraph graph 8 sub1 sub2 in
let _ = print_graph sub1 in
let _ = print_graph sub2 in
let _ = BaseGraph.dump graph sub1 in
let _ = print_graph graph in
let _ = BaseGraph.dump graph sub2 in
let _ = print_graph graph in
()

(* let _ = BaseGraph.merge g 15 8 in
 * BaseGraph.fold_graph (fun _ node -> print_int node; print_string " ") () g; print_string " ";
 * BaseGraph.fold_graph (fun _ node ->
 * print_int node; print_string ": ";
 * BaseGraph.fold_neighbors (fun _ neighbor -> print_int neighbor; print_string ",") () g node;
 * print_string "\n") () g;;  *)
