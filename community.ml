open Graph

module type Community = sig
  type group

  type community

  include Graph

  val make : 'a -> community

  val insert : community -> node -> 'a -> unit

  val init : community -> 'a -> community

  val which_group : community -> node -> group

  val which_nodes : community -> group -> node list

  val in_same_group : community -> node -> node -> bool

  val join : community -> node -> group -> 'a -> unit

  val to_groups : community -> node list list
end

module BaseCommu = struct
  include BaseGraph

  type group = {idx: int; inner: weight; outer: weight}

  type community =
    {nextnode: int; nextgroup: int; hash: (node, group) Hashtbl.t}

  let make (n : int) : community =
    let nextnode = 10 in
    let nextgroup = 10 in
    let hash = Hashtbl.create n in
    {nextnode; nextgroup; hash}

  let insert (c : community) (node : node) (m : int * weight * weight) =
    match m with groupidx, inner, outer ->
      Hashtbl.add c.hash node {idx= groupidx; inner; outer}

  let init (c : community) (init_meta : int * (int -> int -> group)) :
      community =
    let rec init_aux c init_n f iter =
      if init_n = iter then ()
      else
        let _ = Hashtbl.add c iter (f iter iter) in
        init_aux c init_n f (iter + 1)
    in
    match init_meta with init_n, f ->
      let _ = init_aux c.hash init_n f 0 in
      {c with nextnode= init_n; nextgroup= init_n}

  let which_group (c : community) (node : node) : group =
    Hashtbl.find c.hash node

  let which_nodes (c : community) (group : group) : node list =
    Hashtbl.fold
      (fun node group' l -> if group.idx = group'.idx then node :: l else l)
      c.hash []

  let in_same_group (c : community) (node1 : node) (node2 : node) : bool =
    if (which_group c node1).idx = (which_group c node2).idx then true
    else false

  let join (c : community) (node : node) (group_aft : group)
      (f : node -> group * group -> group * group) : unit =
    let group_bef = Hashtbl.find c.hash node in
    let group_bef', group_aft' = f node (group_bef, group_aft) in
    Hashtbl.iter
      (fun node group ->
        if group.idx = group_bef.idx then
          Hashtbl.replace c.hash node group_bef'
        else () ;
        if group.idx = group_aft.idx then
          Hashtbl.replace c.hash node group_aft'
        else () )
      c.hash ;
    Hashtbl.replace c.hash node group_aft'

  let to_groups (c : community) =
    let buffer = Hashtbl.create c.nextgroup in
    let _ =
      Hashtbl.iter
        (fun node group ->
          let og = Hashtbl.find_opt buffer group.idx in
          match og with
          | None ->
              let g = Hashtbl.create c.nextnode in
              let _ = Hashtbl.add g node node in
              Hashtbl.add buffer group.idx g
          | Some g -> Hashtbl.add g node node )
        c.hash
    in
    Hashtbl.fold
      (fun groupidx g ll ->
        let new_l = Hashtbl.fold (fun node node l -> node :: l) g [] in
        new_l :: ll )
      buffer []
end
