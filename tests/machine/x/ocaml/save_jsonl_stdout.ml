(* Generated by Mochi compiler v0.10.27 on 1970-01-01T00:00:00Z *)
let save_jsonl rows path =
  let oc = if path = "-" then stdout else open_out path in
  List.iter (fun m ->
    let parts = List.map (fun (k,v) -> Printf.sprintf "\"%s\": %s" k (__show (Obj.obj v))) m in
    output_string oc ("{" ^ String.concat ", " parts ^ "}\n")
  ) rows;
  if path <> "-" then close_out oc


type record1 = { mutable name : string; mutable age : int }

let people : record1 list = [{ name = "Alice"; age = 30 };{ name = "Bob"; age = 25 }]

let () =
  save_jsonl people "-";
