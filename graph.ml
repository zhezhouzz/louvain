module type Graph = sig
  type weight

  type node

  type nodectx

  type graph

  val length : graph -> int

  val make_graph : 'a -> graph

  val get_weight : graph -> node -> node -> weight

  val get_weight_opt : graph -> node -> node -> weight option

  val get_weight_default : graph -> node -> node -> weight -> weight

  val fold_neighbors :
    ('a -> node -> weight -> 'a) -> 'a -> graph -> node -> 'a

  (* could run in parallel *)

  val compre_neighbors : (node -> weight -> unit) -> graph -> node -> unit

  val fold_graph : ('a -> node -> nodectx -> 'a) -> 'a -> graph -> 'a

  (* could run in parallel *)

  val compre_graph : (node -> nodectx -> unit) -> graph -> unit

  val extract_graph : graph -> node -> graph

  val merge_node_corossgraph : graph -> node -> graph -> graph -> unit
end

module BaseGraph = struct
  type weight = float

  type node = int

  type nodectx = (node, weight) Hashtbl.t

  type graph =
    { mutable inner: weight
    ; mutable outer: weight
    ; hash: (node, nodectx) Hashtbl.t }

  let length (graph : graph) : int = Hashtbl.length graph.hash

  let make_graph (mat : weight option array array) : graph =
    let length = Array.length mat in
    let hash = Hashtbl.create length in
    let rec init_f len idx =
      if len = idx then ()
      else
        let _ = Hashtbl.add hash idx (Hashtbl.create length) in
        init_f len (idx + 1)
    in
    let _ = init_f length 0 in
    let _ =
      Array.fold_left
        (fun i vec ->
          let _ =
            Array.fold_left
              (fun j ow ->
                match ow with
                | None -> j + 1
                | Some w ->
                    let _ = Hashtbl.add (Hashtbl.find hash i) j w in
                    j + 1 )
              0 vec
          in
          i + 1 )
        0 mat
    in
    {inner= 0.0; outer= 0.0; hash}

  (* let get_neighbors (g : graph) (n : node) : node list =
   *   Hashtbl.fold (fun node _ r -> node :: r) (Hashtbl.find g n).weights [] *)

  let get_weight (g : graph) (n_from : node) (n_goto : node) : weight =
    Hashtbl.find (Hashtbl.find g.hash n_from) n_goto

  let get_weight_opt (g : graph) (n_from : node) (n_goto : node) :
      weight option =
    let octx = Hashtbl.find_opt g.hash n_from in
    match octx with None -> None | Some ctx -> Hashtbl.find_opt ctx n_goto

  let get_weight_default (g : graph) (n_from : node) (n_goto : node)
      (default : weight) : weight =
    let ow = get_weight_opt g n_from n_goto in
    match ow with None -> default | Some w -> w

  let fold_neighbors (f : 'a -> node -> weight -> 'a) (start : 'a)
      (graph : graph) (node : node) : 'a =
    let ctx = Hashtbl.find graph.hash node in
    Hashtbl.fold (fun n w r -> f r n w) ctx start

  let compre_neighbors (f : node -> weight -> unit) (graph : graph)
      (node : node) : 'a =
    let ctx = Hashtbl.find graph.hash node in
    Hashtbl.iter (fun n w -> f n w) ctx

  let fold_graph (f : 'a -> node -> nodectx -> 'a) (start : 'a) (g : graph) :
      'a =
    Hashtbl.fold (fun n ctx r -> f r n ctx) g.hash start

  let compre_graph (f : node -> nodectx -> unit) (g : graph) : unit =
    Hashtbl.iter (fun n ctx -> f n ctx) g.hash

  let get_degree (g : graph) (node : node) =
    fold_neighbors (fun sum _ weight -> weight +. sum) 0.0 g node

  let flow_n2ns (graph : graph) (node : node) (nodes : node list) : weight =
    List.fold_left
      (fun sum node' ->
        let w = get_weight_default graph node node' 0.0 in
        w +. sum )
      0.0 nodes

  let flow_n2g (graph : graph) (node : node) (sub_graph : graph) : weight =
    fold_graph
      (fun sum node' _ ->
        let w = get_weight_default graph node node' 0.0 in
        w +. sum )
      0.0 sub_graph

  let extract_graph (graph : graph) (node : node) : graph =
    let hash = Hashtbl.create (length graph) in
    let ctx = Hashtbl.create (length graph) in
    let ow = get_weight_opt graph node node in
    let _ = match ow with None -> () | Some w -> Hashtbl.add ctx node w in
    let _ = Hashtbl.add hash node ctx in
    let inner = get_weight_default graph node node 0.0 in
    {inner; outer= get_degree graph node -. inner; hash}

  let merge_node_corssgraph (graph : graph) (node : node)
      (origin_graph : graph) (dest_graph : graph) : unit =
    let degree = get_degree graph node in
    let self = get_weight_default graph node node 0.0 in
    let delta_inner = flow_n2g graph node origin_graph in
    let delta_outer = degree -. delta_inner -. (delta_inner -. self) in
    let _ = origin_graph.inner <- origin_graph.inner -. delta_inner in
    let _ = origin_graph.outer <- origin_graph.outer -. delta_outer in
    let ctx = Hashtbl.find graph.hash node in
    (* let ctx_origin = Hashtbl.find origin_graph.hash node in *)
    let _ =
      Hashtbl.iter
        (fun node' ctx' -> Hashtbl.remove ctx' node)
        origin_graph.hash
    in
    let _ = Hashtbl.remove origin_graph.hash node in
    let ctx_dest = Hashtbl.create (Hashtbl.length ctx) in
    let _ =
      Hashtbl.iter
        (fun node' ctx' ->
          let ow = get_weight_opt graph node node' in
          match ow with
          | None -> ()
          | Some w ->
              let _ = Hashtbl.add ctx' node w in
              Hashtbl.add ctx_dest node' w )
        dest_graph.hash
    in
    let _ = Hashtbl.add dest_graph.hash node ctx_dest in
    let delta_inner' = flow_n2g graph node dest_graph in
    let delta_outer' = degree -. delta_inner' -. (delta_inner' -. self) in
    let _ = dest_graph.inner <- dest_graph.inner -. delta_inner' in
    let _ = dest_graph.outer <- dest_graph.outer -. delta_outer' in
    ()

  let dump (graph : graph) (sub_graph : graph) : unit =
    let len = length sub_graph in
    if len <= 1 then ()
    else
      let head = fold_graph (fun _ node _ -> node) 0 sub_graph in
      let new_ctx = Hashtbl.create (length graph) in
      let _ =
        compre_graph
          (fun node ctx' ->
            if Hashtbl.mem sub_graph.hash node then ()
            else
              let new_w = flow_n2g graph node sub_graph in
              let _ = Hashtbl.add new_ctx node new_w in
              let _ =
                Hashtbl.iter
                  (fun node _ ->
                    if Hashtbl.mem sub_graph.hash node then
                      Hashtbl.remove ctx' node
                    else () )
                  ctx'
              in
              Hashtbl.add ctx' head new_w )
          graph
      in
      compre_graph
        (fun node _ ->
          if node = head then Hashtbl.replace graph.hash head new_ctx
          else Hashtbl.remove graph.hash node )
        sub_graph
end
