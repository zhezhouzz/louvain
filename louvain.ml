open Graph;;
open Community;;

let cal_sigma_in (group: BaseCommu.group) =
group.inner;;

let cal_sigma_tot (group: BaseCommu.group) =
group.outer +. group.inner;;

let cal_k_i graph node =
BaseGraph.fold_neighbors (fun sum neighbor ->
(* let _ = Printf.printf "<%i - %i>" node neighbor in *)
let e = BaseGraph.get_edage graph node neighbor in
sum +. e.weight) 0.0 graph node;;

let cal_k_i_in graph node group commu =
let nodes_in_commu = BaseCommu.which_nodes commu group in
List.fold_left (fun sum node_in_commu ->
let ow = BaseGraph.get_weight_opt graph node node_in_commu in
match ow with
| None -> sum
| Some w -> sum +. w
) 0.0 nodes_in_commu;;

let cal_m graph =
(BaseGraph.fold_graph (fun sum node ->
BaseGraph.fold_neighbors (fun sum' neighbor ->
let ow = BaseGraph.get_weight_opt graph node neighbor in
match ow with
| None -> sum'
| Some w -> sum' +. w
) sum graph node
) 0.0 graph) /. 2.0;;

let cal_Q graph commu =
let m = cal_m graph in
let sum = BaseGraph.fold_graph (fun sum node1 ->
BaseGraph.fold_graph (fun sum' node2 ->
(* let _ = Printf.printf "sum = %f\n" sum' in *)
if (BaseCommu.in_same_group commu node1 node2) then
let ow = BaseGraph.get_weight_opt graph node1 node2 in
let weight =
match ow with
| None -> 0.0
| Some w -> w in
let ki = cal_k_i graph node1 in
let kj = cal_k_i graph node2 in
(* let _ = Printf.printf "<%i,%i>: ki = %f kj = %f\n" node1 node2 ki kj in *)
sum' +. (weight -. (ki *. kj /. (2.0 *. m)))
else sum'
) sum graph
) 0.0 graph in
sum /. (2.0 *. m);;

let mysqrt x = x*.x;;

let cal_delta_Q_aux sigma_in sigma_tot k_i k_i_in m =
let expr1 = (sigma_in +. 2.0 *. k_i_in) /. (2.0 *. m) in
let expr2 = mysqrt ((sigma_tot +. k_i) /. (2.0 *. m)) in
let expr3 = sigma_in /. (2.0 *. m) in
let expr4 = mysqrt (sigma_tot /. (2.0 *. m)) in
let expr5 = mysqrt (k_i /. (2.0 *. m)) in
let _ = Printf.printf "expr1 = %f; expr2 = %f; expr3 = %f; expr4 = %f; expr5 = %f\n" expr1 expr2 expr3 expr4 expr5 in 
expr1 -. expr2 -. (expr3 -. expr4 -. expr5);;

let cal_delta_Q graph node group commu =
let sigma_in = cal_sigma_in group in
let sigma_tot = cal_sigma_tot group in
let k_i = cal_k_i graph node in
let k_i_in = cal_k_i_in graph node group commu in
let m = cal_m graph in
let _ = Printf.printf "sigma_in = %f; sigma_tot = %f; k_i = %f; k_i_in = %f; m = %f\n" sigma_in sigma_tot k_i k_i_in m in
cal_delta_Q_aux sigma_in sigma_tot k_i k_i_in m;;
(* cal_delta_Q_aux sigma_in sigma_tot k_i k_i_in m;; *)
(* let expr1 = (sigma_in +. 2.0 *. k_i_in) /. (2.0 *. m) in
 * let expr2 = mysqrt ((sigma_tot +. k_i) /. (2.0 *. m)) in
 * let expr3 = sigma_in /. (2.0 *. m) in
 * let expr4 = mysqrt (sigma_tot /. (2.0 *. m)) in
 * let expr5 = mysqrt (k_i /. (2.0 *. m)) in
 * let _ = Printf.printf "expr1 = %f; expr2 = %f; expr3 = %f; expr4 = %f; expr5 = %f\n" expr1 expr2 expr3 expr4 expr5 in 
 * expr1 -. expr2 -. (expr3 -. expr4 -. expr5);; *)

let cal_delta_Q_move graph node group commu =
let k_i_in = cal_k_i_in graph node group commu in
let k_i = cal_k_i graph node in
let sigma_in = (cal_sigma_in group) -. k_i_in in
let sigma_tot = (cal_sigma_tot group) -. (k_i -. k_i_in) +. k_i_in in
let m = cal_m graph in
let _ = Printf.printf "sigma_in = %f; sigma_tot = %f; k_i = %f; k_i_in = %f; m = %f\n" sigma_in sigma_tot k_i k_i_in m in 
let expr1 = (sigma_in +. 2.0 *. k_i_in) /. (2.0 *. m) in
let expr2 = mysqrt ((sigma_tot +. k_i) /. (2.0 *. m)) in
let expr3 = sigma_in /. (2.0 *. m) in
let expr4 = mysqrt (sigma_tot /. (2.0 *. m)) in
let expr5 = mysqrt (k_i /. (2.0 *. m)) in
let _ = Printf.printf "expr1 = %f; expr2 = %f; expr3 = %f; expr4 = %f; expr5 = %f\n" expr1 expr2 expr3 expr4 expr5 in 
expr1 -. expr2 -. (expr3 -. expr4 -. expr5);;

let flow_node_to_nodes graph node nodes =
List.fold_left (fun sum neighbor ->
let ow = BaseGraph.get_weight_opt graph node neighbor in
match ow with
| None -> sum
| Some w -> sum +. w) 0.0 nodes;;

let louvain_join g commu node dest =
BaseCommu.join commu node (BaseCommu.which_group commu dest) (fun node (group_from, group_to) ->
let inner_flow = flow_node_to_nodes g node (BaseCommu.which_nodes commu group_from) in
let outer_flow = (cal_k_i g node) -. inner_flow in
let inner_flow' = flow_node_to_nodes g node (BaseCommu.which_nodes commu group_to) in
let outer_flow' = (cal_k_i g node) -. inner_flow' in
({idx = group_from.idx;
inner = group_from.inner -. inner_flow;
outer = group_from.outer -. outer_flow +. inner_flow},
{idx = group_to.idx;
inner = group_to.inner +. inner_flow';
outer = group_to.outer +. outer_flow' -. inner_flow'}));;

let test_data = [|
[|None; Some 1.0; Some 1.0|];
[|Some 1.0; None; None|];
[|Some 1.0; None; None|];
|] in
let g = BaseGraph.make_graph test_data in
let _ = Printf.printf "m = %f\n" (cal_m g) in
let init_f node groupid : BaseCommu.group=
{idx = groupid; inner = 0.0; outer = cal_k_i g node} in
let test_c = BaseCommu.init (BaseCommu.make 1024) (3, init_f) in
let _ = Printf.printf "Q = %f\n" (cal_Q g test_c) in
let _ = Printf.printf "delta_Q (1 -> 2) = %f\n" (cal_delta_Q g 1 (BaseCommu.which_group test_c 2) test_c) in
let _ = Printf.printf "delta_Q_move (1 -> 1) = %f\n" (cal_delta_Q_move g 1 (BaseCommu.which_group test_c 1) test_c) in
let _ = louvain_join g test_c 1 2 in
let _ = Printf.printf "Q = %f\n" (cal_Q g test_c) in
let _ = Printf.printf "delta_Q (1 -> 0) = %f\n" (cal_delta_Q g 1 (BaseCommu.which_group test_c 0) test_c) in
let _ = Printf.printf "delta_Q_move (1 -> 1) = %f\n" (cal_delta_Q_move g 1 (BaseCommu.which_group test_c 1) test_c) in
let _ = louvain_join g test_c 1 0 in
let _ = Printf.printf "Q = %f\n" (cal_Q g test_c) in ();;

