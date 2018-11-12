open Graph;;

module type Community = sig
type group
type community
include Graph

val make : 'a -> community
val init : community -> 'a -> community
val which_group : community -> node -> group
val which_nodes : community -> group -> node list
val join : community -> node -> group -> 'a -> unit
end;;

module BaseCommu = struct
include BaseGraph
type group = {idx: int; inner: int; outer: int}
type community = {nextnode: int; nextgroup: int; hash:(node, group) Hashtbl.t}

let make (n: int) : community =
let nextnode = 0 in
let nextgroup = 0 in
let hash = Hashtbl.create n in
{nextnode = nextnode; nextgroup = nextgroup; hash = hash};;

let init (c: community) (init_meta: int * (int -> int-> group)): community =
let rec init_aux c init_n f iter =
if init_n = iter then () else
let _ = Hashtbl.add c iter (f iter iter) in init_aux c init_n f (iter+1) in
match init_meta with | (init_n, f) ->
let _ = print_int init_n in
let _ = init_aux c.hash init_n f 0 in
{c with nextnode = init_n; nextgroup = init_n};;

let which_group (c: community) (node: node): group =
Hashtbl.find c.hash node;;

let which_nodes (c: community) (group: group) : node list =
Hashtbl.fold (fun node group' l ->
if group.idx = group'.idx then node::l else l) c.hash [];;

let join (c: community) (node: node) (group_aft: group) (f: node -> group* group -> group* group): unit =
let group_bef = Hashtbl.find c.hash node in
let (group_bef', group_aft') = f node (group_bef, group_aft) in
Hashtbl.iter (fun node group ->
if group.idx = group_bef.idx then Hashtbl.replace c.hash node group_bef' else ();
if group.idx = group_aft.idx then Hashtbl.replace c.hash node group_aft' else ()) c.hash;
Hashtbl.replace c.hash node group_aft';;

end;;

let init_f node groupid : BaseCommu.group=
{idx = groupid; inner = 0; outer = 0} in
let test_c = BaseCommu.init (BaseCommu.make 1024) (16, init_f) in
let group = BaseCommu.which_group test_c 3 in
let _ = Printf.printf "node: {%i, %i, %i}\n" group.idx group.inner group.outer in
let nodes = BaseCommu.which_nodes test_c group in
let _ = List.fold_left (fun _ elem ->
let _ = Printf.printf "node(%i) " elem in ()) () nodes in ();;
