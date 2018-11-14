open Graph

module type Community = sig
  type element

  type group

  type community

  val create : int -> community

  val add : community -> group -> unit

  val which_group : community -> element -> group

  val in_same_group : community -> element -> element -> bool

  val fold_commu : ('a -> group -> 'a) -> 'a -> community -> 'a

  val compre_commu : (group -> unit) -> community -> unit
end

module BaseCommu = struct
  include BaseGraph

  type element = node

  type group = graph

  type community = (int, graph) Hashtbl.t

  let create (init_length : int) : community = Hashtbl.create init_length

  let add (commu : community) (graph : graph) =
    let idx = Hashtbl.length commu in
    Hashtbl.add commu idx graph

  let fold_commu (f : 'a -> group -> 'a) (start : 'a) (commu : community) : 'a
      =
    Hashtbl.fold (fun _ group r -> f r group) commu start

  let compre_commu (f : group -> unit) (commu : community) : unit =
    Hashtbl.iter (fun _ group -> f group) commu

  let which_group_opt (commu : community) (elem : element) : group option =
    fold_commu
      (fun og group ->
        match og with
        | Some g -> og
        | None -> if Hashtbl.mem group.hash elem then Some group else None )
      None commu

  exception Not_belong_to_any_group

  let which_group (commu : community) (elem : element) : group =
    match which_group_opt commu elem with
    | None -> raise Not_belong_to_any_group
    | Some g -> g

  let in_same_group (commu : community) (node1 : element) (node2 : element) :
      bool =
    let graph = which_group commu node1 in
    Hashtbl.mem graph.hash node2
end
