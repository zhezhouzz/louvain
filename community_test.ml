open Community;;

let init_f node groupid : BaseCommu.group=
{idx = groupid; inner = 0.0; outer = 0.0} in
let test_c = BaseCommu.init (BaseCommu.make 1024) (16, init_f) in
let group = BaseCommu.which_group test_c 3 in
let _ = Printf.printf "node: {%i, %f, %f}\n" group.idx group.inner group.outer in
let nodes = BaseCommu.which_nodes test_c group in
let _ = List.fold_left (fun _ elem ->
let _ = Printf.printf "node(%i) " elem in ()) () nodes in ();;
