module type Graph = sig
type node
type edage
type graph

val make_graph : 'a -> graph
val get_neighbors : graph -> node -> node list
val get_edage : graph -> node -> node -> edage
val fold_neighbors : ('a -> node -> 'a) -> 'a -> graph -> node -> 'a
val fold_graph : ('a -> node -> 'a) -> 'a -> graph -> 'a
val merge : graph -> node -> node -> unit
end;;

module BaseGraph = struct
type weight = int
type node = int
type edage = {from: node; goto: node; weight: int}
type node_context = {self : node; edages: (node, weight) Hashtbl.t}
type graph = (node, node_context) Hashtbl.t


(* let make_node_outcontext (mat: (weight option) array array): (node, node_context) Hashtbl.t=
 * let length = Array.length mat in
 * let ctxs = Hashtbl.create length in
 * let rec init_f len idx =
 * if len = idx then () else
 * let _ = Hashtbl.add ctxs idx {self = idx; edages = (Hashtbl.create length)} in init_f len (idx + 1) in
 * let _ = init_f length 0 in
 * let _ = Array.fold_left (fun i vec ->
 * let _ = Array.fold_left (fun j ow ->
 * match ow with
 * | None -> j + 1
 * | Some w -> let _ = Hashtbl.add (Hashtbl.find ctxs j).edages i w in j + 1
 * ) 0 vec in i + 1
 * ) 0 mat in ctxs;; *)

let make_node_context (mat: (weight option) array array): (node, node_context) Hashtbl.t=
let length = Array.length mat in
let ctxs = Hashtbl.create length in
let rec init_f len idx =
if len = idx then () else
let _ = Hashtbl.add ctxs idx {self = idx; edages = (Hashtbl.create length)} in init_f len (idx + 1) in
let _ = init_f length 0 in
let _ = Array.fold_left (fun i vec ->
let _ = Array.fold_left (fun j ow ->
match ow with
| None -> j + 1
| Some w -> let _ = Hashtbl.add (Hashtbl.find ctxs i).edages j w in j + 1
) 0 vec in i + 1
) 0 mat in ctxs;;

let make_graph (mat: (weight option) array array) : graph =
make_node_context mat

let get_neighbors (g: graph) (n: node) : node list =
Hashtbl.fold (fun node _ r -> node :: r) (Hashtbl.find g n).edages []

let get_edage (g: graph) (n_from: node) (n_goto : node) : edage =
{from = n_from; goto = n_goto; weight = Hashtbl.find (Hashtbl.find g n_from).edages n_goto};;

let get_weight_opt (g: graph) (n_from: node) (n_goto : node) : weight option=
Hashtbl.find_opt (Hashtbl.find g n_from).edages n_goto;;

let fold_neighbors (f: 'a -> node -> 'a)  (start: 'a) (g: graph) (n: node): 'a =
let neighbors = get_neighbors g n in
List.fold_left (fun r n -> f r n) start neighbors;;

let fold_graph (f: 'a -> node -> 'a) (start: 'a) (g: graph) : 'a =
Hashtbl.fold (fun n ctx r-> f r n) g start;;

(* let merge (graph:graph) (tomerge: node) (dest: node) : unit =
 * Hashtbl.fold (fun neighbor w1 r ->
 * let ow = get_weight_opt graph dest neighbor in
 * match ow with
 * | None -> 
 * let _ = Hashtbl.set (Hashtbl.find g.incontext neighbor) dest w1 in
 * let _ = Hashtbl.set (Hashtbl.find g.incontext neighbor) tomerge 0 in
 * 
 * | Some w2 ->
 * ) (Hashtbl.find g.incontext tomerge) _
 * ) _ neighbors   *)

end;;
