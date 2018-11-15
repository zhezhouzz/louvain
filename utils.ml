open Graph
open Community

let print_graph graph =
  let _ =
    print_string "======================================================\n"
  in
  let _ =
    BaseGraph.fold_graph
      (fun _ node _ ->
        let _ = Printf.printf "[Graph][Node(%i)]: " node in
        let _ =
          BaseGraph.fold_neighbors
            (fun _ neighbor w ->
              let _ = Printf.printf "{%i -> %i = %f} " node neighbor w in
              () )
            () graph node
        in
        print_string "\n" )
      () graph
  in
  let inner = BaseGraph.get_inner graph in
  let outer = BaseGraph.get_outer graph in
  let _ = Printf.printf "Inner = %f; Outer = %f\n" inner outer in
  print_string "======================================================\n"

let print_commu_state commu =
  let _ = print_string "<---- PRINT_COMMU_STATE ---->\n" in
  let _ =
    BaseCommu.compre_commu
      (fun graph ->
        if BaseGraph.length graph = 0 then () else print_graph graph )
      commu
  in
  let _ = print_string "<---- PRINT_COMMU_STATE ---->\n" in
  ()
