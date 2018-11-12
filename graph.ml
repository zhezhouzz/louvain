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
type graph = {in_context: node_context array; out_context: node_context array}


let make_node_outcontext (mat: (weight option) array array): node_context array=
let length = Array.length mat in
let ctxs = Array.init length (fun idx -> {self = idx; edages = (Hashtbl.create length)}) in
let _ = Array.fold_left (fun i vec ->
let _ = Array.fold_left (fun j ow ->
match ow with
| None -> j + 1
| Some w -> let _ = Hashtbl.add ctxs.(j).edages i w in j + 1
) 0 vec in i + 1
) 0 mat in ctxs;;

let make_node_incontext (mat: (weight option) array array): node_context array=
let length = Array.length mat in
let ctxs = Array.init length (fun idx -> {self = idx; edages = Hashtbl.create length}) in
let _ = Array.fold_left (fun i vec ->
let _ = Array.fold_left (fun j ow ->
match ow with
| None -> j + 1
| Some w -> let _ = Hashtbl.add ctxs.(i).edages j w in j + 1
) 0 vec in i + 1
) 0 mat in ctxs;;

(* let make_node_incontext (mat: (weight option) array array): node_context array=
 * let length = Array.length mat in
 * let node_list_array = Array.init length (fun idx -> (idx, [])) in
 * (\* let _ = Array.fold_left (fun _ (x, _) -> print_int x; print_string "*") () node_list_array in *\)
 * let (node_list_array', _) = Array.fold_left (fun (ctx, i) vec ->
 * let (ctx', _) = Array.fold_left (fun (ctx, j) ow ->
 * match ow with
 * | None -> (ctx, j + 1)
 * | Some w ->
 * match ctx.(i) with
 * | (fst, snd) -> Array.set ctx i (fst, (make_edage i j w) :: snd); (ctx, j + 1)
 * ) (ctx, 0) vec in
 * (ctx', i + 1)
 * ) (node_list_array, 0) mat in
 * Array.map (fun (fst, snd) ->
 * {self = fst; edages = Array.of_list snd}) node_list_array';; *)


let make_graph (mat: (weight option) array array) : graph =
let in_context = make_node_incontext mat in
let out_context = make_node_outcontext mat in
{in_context = in_context; out_context = out_context};;


let get_neighbors (g: graph) (n: node) : node list =
let node_from_me = Hashtbl.fold (fun node _ r -> node :: r) g.in_context.(n).edages [] in 
let node_goto_me = Hashtbl.fold (fun node _ r -> node :: r) g.out_context.(n).edages [] in
node_goto_me@node_from_me;;

let get_edage (g: graph) (n_from: node) (n_goto : node) : edage =
{from = n_from; goto = n_goto; weight = Hashtbl.find g.in_context.(n_from).edages n_goto};;

let fold_neighbors (f: 'a -> node -> 'a)  (start: 'a) (g: graph) (n: node): 'a =
let neighbors = get_neighbors g n in
List.fold_left (fun r n -> f r n) start neighbors;;

let fold_graph (f: 'a -> node -> 'a) (start: 'a) (g: graph) : 'a =
Array.fold_left (fun r ctx -> f r ctx.self) start g.in_context;;

(* let merge (graph:graph) (node: node) (destination: node) : unit = *)


end;;
