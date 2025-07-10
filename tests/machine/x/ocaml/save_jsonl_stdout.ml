let save_jsonl rows path =
  let oc = if path = "-" then stdout else open_out path in
  List.iter (fun m ->
    let parts = List.map (fun (k,v) -> Printf.sprintf "\"%s\": %s" k (__show (Obj.obj v))) m in
    output_string oc ("{" ^ String.concat ", " parts ^ "}\n")
  ) rows;
  if path <> "-" then close_out oc


let people = [[("name",Obj.repr ("Alice"));("age",Obj.repr (30))];[("name",Obj.repr ("Bob"));("age",Obj.repr (25))]]

let () =
  save_jsonl people "-";
