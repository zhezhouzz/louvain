open Graph;;

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
              let _ =
                Printf.printf "{%i -> %i = %f} " node neighbor w
              in
              () )
            () graph node
        in
        print_string "\n" )
      () graph
  in
  let inner = BaseGraph.get_inner graph in
  let outer = BaseGraph.get_outer graph in
  let _ = Printf.printf "Inner = %f; Outer = %f\n" inner outer in
  print_string "======================================================\n";;
