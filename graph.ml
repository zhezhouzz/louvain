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
type graph = {in_context: (node, node_context) Hashtbl.t; out_context: (node, node_context) Hashtbl.t}


let make_node_outcontext (mat: (weight option) array array): (node, node_context) Hashtbl.t=
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
| Some w -> let _ = Hashtbl.add (Hashtbl.find ctxs j).edages i w in j + 1
) 0 vec in i + 1
) 0 mat in ctxs;;

let make_node_incontext (mat: (weight option) array array): (node, node_context) Hashtbl.t=
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
let in_context = make_node_incontext mat in
let out_context = make_node_outcontext mat in
{in_context = in_context; out_context = out_context};;


let get_neighbors (g: graph) (n: node) : node list =
let node_from_me = Hashtbl.fold (fun node _ r -> node :: r) (Hashtbl.find g.in_context n).edages [] in 
let node_goto_me = Hashtbl.fold (fun node _ r -> node :: r) (Hashtbl.find g.out_context n).edages [] in
node_goto_me@node_from_me;;

let get_edage (g: graph) (n_from: node) (n_goto : node) : edage =
{from = n_from; goto = n_goto; weight = Hashtbl.find (Hashtbl.find g.in_context n_from).edages n_goto};;

let fold_neighbors (f: 'a -> node -> 'a)  (start: 'a) (g: graph) (n: node): 'a =
let neighbors = get_neighbors g n in
List.fold_left (fun r n -> f r n) start neighbors;;

let fold_graph (f: 'a -> node -> 'a) (start: 'a) (g: graph) : 'a =
Hashtbl.fold (fun n ctx r-> f r n) g.in_context start;;

(* let merge (graph:graph) (node: node) (destination: node) : unit = *)


end;;
