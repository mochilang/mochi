    let rec __show v =
      let open Obj in
      let rec list_aux o =
        if is_int o && (magic (obj o) : int) = 0 then "" else
         let hd = field o 0 in
         let tl = field o 1 in
         let rest = list_aux tl in
         if rest = "" then __show (obj hd) else __show (obj hd) ^ "; " ^ rest
      in
      let r = repr v in
      if is_int r then string_of_int (magic v) else
      match tag r with
        | 0 -> if size r = 0 then "[]" else "[" ^ list_aux r ^ "]"
        | 252 -> (magic v : string)
        | 253 -> string_of_float (magic v)
        | _ -> "<value>"

    exception Break
    exception Continue

    type ('k,'v) group = { key : 'k; items : 'v list }

    type record1 = { mutable tag : string; mutable val : int }
    type record2 = { mutable tag : Obj.t; mutable total : Obj.t }

let data : record1 list = [{ tag = "a"; val = 1 };{ tag = "a"; val = 2 };{ tag = "b"; val = 3 }]
let groups : (Obj.t,(string * Obj.t) list) group list = (let __groups0 = ref [] in
  List.iter (fun d ->
      let key = Obj.obj (List.assoc "tag" d) in
      let cur = try List.assoc key !__groups0 with Not_found -> [] in
      __groups0 := (key, d :: cur) :: List.remove_assoc key !__groups0;
  ) data;
  let __res0 = ref [] in
  List.iter (fun (gKey,gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res0 := g :: !__res0
  ) !__groups0;
  List.rev !__res0)

let tmp : Obj.t list ref = ref []
let result : Obj.t list = (let __res1 = ref [] in
  List.iter (fun r ->
      __res1 := r :: !__res1;
  ) (!tmp);
List.rev !__res1)


let () =
  let rec __loop2 lst =
    match lst with
      | [] -> ()
      | g::rest ->
        try
          let total : int ref = ref 0 in
          let rec __loop3 lst =
            match lst with
              | [] -> ()
              | x::rest ->
                try
                  total := ((!total) + Obj.obj (List.assoc "val" x));
                with Continue -> ()
                ; __loop3 rest
            in
            try __loop3 g.items with Break -> ()
            tmp := ((!tmp) @ [{ tag = g.key; total = (!total) }]);
          with Continue -> ()
          ; __loop2 rest
      in
      try __loop2 groups with Break -> ()
      print_endline (__show (result));
