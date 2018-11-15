open Graph
open Community
open Utils
open Data

let cal_Q graph commu =
  let two_m = BaseGraph.get_inner graph in
  let sum =
    BaseGraph.fold_graph
      (fun sum i _ ->
        BaseGraph.fold_graph
          (fun sum' j _ ->
            (* let _ = Printf.printf "sum = %f\n" sum' in *)
            if BaseCommu.in_same_group commu i j then
              let a_i_j = BaseGraph.get_weight_default graph i j 0.0 in
              let ki = BaseGraph.get_degree graph i in
              let kj = BaseGraph.get_degree graph j in
              (* let _ = Printf.printf "<%i,%i>: ki = %f kj = %f\n" node1 node2 ki kj in *)
              sum' +. (a_i_j -. (ki *. kj /. two_m))
            else sum' )
          sum graph )
      0.0 graph
  in
  sum /. two_m

let mysqrt x = x *. x

let cal_delta_Q_aux sigma_in sigma_tot k_i k_i_in two_m =
  let expr1 = (sigma_in +. (2.0 *. k_i_in)) /. two_m in
  let expr2 = mysqrt ((sigma_tot +. k_i) /. two_m) in
  let expr3 = sigma_in /. two_m in
  let expr4 = mysqrt (sigma_tot /. two_m) in
  let expr5 = mysqrt (k_i /. two_m) in
  (* let _ =
   *   Printf.printf
   *     "expr1 = %f; expr2 = %f; expr3 = %f; expr4 = %f; expr5 = %f\n" expr1
   *     expr2 expr3 expr4 expr5
   * in *)
  expr1 -. expr2 -. (expr3 -. expr4 -. expr5)

let cal_delta_Q graph i graph_dest =
  let sigma_in = BaseGraph.get_inner graph_dest in
  let sigma_tot = (BaseGraph.get_outer graph_dest) +. sigma_in in
  let k_i = BaseGraph.get_degree graph i in
  let k_i_in = BaseGraph.flow_n2g_default graph i graph_dest 0.0 in
  let two_m = BaseGraph.get_inner graph in
  (* let _ =
   *   Printf.printf
   *     "sigma_in = %f; sigma_tot = %f; k_i = %f; k_i_in = %f; m = %f\n" sigma_in
   *     sigma_tot k_i k_i_in m
   * in *)
  cal_delta_Q_aux sigma_in sigma_tot k_i k_i_in two_m

let n2g_notself node sub_graph =
BaseGraph.fold_graph (fun sum node' _ ->
let w = BaseGraph.get_weight_default sub_graph node node' 0.0 in
let w' = if node = node' then 0.0 else w in
sum +. w'
) 0.0 sub_graph


let cal_delta_Q_move graph i graph_origin =
  let k_i_i = BaseGraph.get_weight_default graph_origin i i 0.0 in
  let k_i_in = n2g_notself i graph_origin in
  let k_i =  BaseGraph.get_degree graph_origin i in
  let sigma_in = (BaseGraph.get_inner graph_origin) -. (2.0 *. k_i_i) -. k_i_in in
  let k_i_out = k_i -. k_i_in -. k_i_i in
  let sigma_tot = (BaseGraph.get_inner graph_origin) +. (BaseGraph.get_outer graph_origin) -. k_i_out +. k_i_in -. (2.0 *. k_i_i) in
  let two_m = BaseGraph.get_inner graph in
  (* let _ =
   *   Printf.printf
   *     "sigma_in = %f; sigma_tot = %f; k_i = %f; k_i_in = %f; two_m = %f\n" sigma_in
   *     sigma_tot k_i k_i_in two_m
   * in *)
  cal_delta_Q_aux sigma_in sigma_tot k_i k_i_in two_m


let find_best_neighbor graph commu node =
  let best_neighbor =
    BaseGraph.fold_neighbors
      (fun o neighbor _ ->
        let _ = Printf.printf "best: node(%i) neighbor(%i)\n" node neighbor in
        if BaseCommu.in_same_group commu node neighbor then o
        else
          let delta_Q =
            cal_delta_Q graph node (BaseCommu.which_group commu neighbor)
          in
          let delta_Q_move =
            cal_delta_Q_move graph node
              (BaseCommu.which_group commu node)
          in
          let delta_Q_tot = delta_Q -. delta_Q_move in
          let _ =
            Printf.printf "%i -> %i: (%f - %f) = %f\n" node neighbor delta_Q
              delta_Q_move delta_Q_tot
          in
          match o with
          | None -> Some (neighbor, delta_Q_tot)
          | Some (n, m) ->
              if m < delta_Q_tot then Some (neighbor, delta_Q_tot)
              else Some (n, m) )
      None graph node
  in
  match best_neighbor with
  | None -> None
  | Some (n, m) -> if m > 0.0 then Some n else None

let print_commu_state commu =
let _ = print_string "<---- PRINT_COMMU_STATE ---->\n" in
let _ = BaseCommu.compre_commu (fun graph ->
if BaseGraph.length graph = 0 then ()
else print_graph graph 
) commu in
let _ = print_string "<---- PRINT_COMMU_STATE ---->\n" in
()

let rec phase1 graph commu =
  let if_convergence =
    BaseGraph.fold_graph
      (fun if_ node _ ->
        let _ = Printf.printf "phase1: node(%i)\n" node in
        let best_neighbor = find_best_neighbor graph commu node in
        match best_neighbor with
        | None -> if_
        | Some neighbor ->
            let _ = BaseGraph.merge_node_crossgraph graph node (BaseCommu.which_group commu node) (BaseCommu.which_group commu neighbor) in
(* let _ = print_commu_state commu in *)
            false )
      true graph
  in
  if if_convergence then () else phase1 graph commu

let phase2 graph commu =
BaseCommu.compre_commu (fun sub_graph ->
BaseGraph.dump graph sub_graph
) commu

let rec louvain_loop graph =
  let len = BaseGraph.length graph in 
  let commu = BaseCommu.create len in
let _ = BaseGraph.compre_graph (fun node _ ->
let sub_graph = BaseGraph.extract_graph graph node in
BaseCommu.add commu sub_graph
) graph in
  let _ = phase1 graph commu in
  let _ = phase2 graph commu in
  let _ = print_graph graph in
  (* let _ = print_commu_state commu in *)
  let _ = Printf.printf "Q = %f\n" (cal_Q graph commu) in
  let len' = BaseGraph.length graph in
  if len' = len then () else louvain_loop graph

;;


let g = BaseGraph.make_graph test_data_16 in
louvain_loop g;;

(* let init_f node groupid : BaseCommu.group =
 *   {idx= groupid; inner= 0.0; outer= cal_k_i g node}
 * in
 * let test_c = BaseCommu.init (BaseCommu.make 1024) (BaseGraph.length g, init_f) in
 * let _ = Printf.printf "delta_Q (0 -> 2) = %f\n" (cal_delta_Q g 0 (BaseCommu.which_group test_c 2) test_c) in
 * let _ = phase1 g test_c in
 * let groups = BaseCommu.to_groups test_c in
 * let _ = print_ll groups in
 * let _ = print_commu_state g test_c in
 * let _ = phase2 g test_c in
 * print_graph g;; *)

(* let _ = Printf.printf "Q = %f\n" (cal_Q g test_c) in
 * let _ = Printf.printf "delta_Q (1 -> 2) = %f\n" (cal_delta_Q g 1 (BaseCommu.which_group test_c 2) test_c) in
 * let _ = Printf.printf "delta_Q_move (1 -> 1) = %f\n" (cal_delta_Q_move g 1 (BaseCommu.which_group test_c 1) test_c) in
 * let _ = louvain_join g test_c 1 2 in
 * let _ = Printf.printf "Q = %f\n" (cal_Q g test_c) in
 * let _ = Printf.printf "delta_Q (1 -> 0) = %f\n" (cal_delta_Q g 1 (BaseCommu.which_group test_c 0) test_c) in
 * let _ = Printf.printf "delta_Q_move (1 -> 1) = %f\n" (cal_delta_Q_move g 1 (BaseCommu.which_group test_c 1) test_c) in
 * let _ = louvain_join g test_c 1 0 in
 * let _ = Printf.printf "Q = %f\n" (cal_Q g test_c) in ();; *)
