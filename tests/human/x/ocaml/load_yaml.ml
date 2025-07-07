(* Hand-written OCaml version of load_yaml.mochi *)

(* simple parser for the YAML file used in the example *)

type person = { name:string; age:int; email:string }

let parse_people path =
  let ic = open_in path in
  let rec loop acc current =
    match input_line ic with
    | line ->
        let line = String.trim line in
        if line = "" then loop acc current
        else
          let idx = try String.index line ':' with Not_found -> -1 in
          if idx < 0 then loop acc current else
          let key = String.sub line 0 idx |> String.trim in
          let value = String.sub line (idx+1) (String.length line - idx - 1) |> String.trim in
          if key = "- name" then
            let acc = (match current with None -> acc | Some p -> p :: acc) in
            let p = { name=value; age=0; email="" } in
            loop acc (Some p)
          else if key = "age" then
            let current = match current with
              | None -> { name=""; age=int_of_string value; email="" }
              | Some p -> { p with age=int_of_string value } in
            loop acc (Some current)
          else if key = "email" then
            let current = match current with
              | None -> { name=""; age=0; email=value }
              | Some p -> { p with email=value } in
            loop acc (Some current)
          else loop acc current
    | exception End_of_file ->
        close_in ic;
        let acc = match current with None -> acc | Some p -> p :: acc in
        List.rev acc
  in
  loop [] None

let () =
  let people = parse_people "tests/interpreter/valid/people.yaml" in
  List.iter (fun p ->
    if p.age >= 18 then Printf.printf "%s %s\n" p.name p.email
  ) people
