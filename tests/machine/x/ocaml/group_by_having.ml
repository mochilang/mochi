type ('k,'v) group = { key : 'k; items : 'v list }

type record1 = { mutable name : string; mutable city : string }
type record2 = { mutable city : Obj.t; mutable num : int }

let people : record1 list = [{ name = "Alice"; city = "Paris" };{ name = "Bob"; city = "Hanoi" };{ name = "Charlie"; city = "Paris" };{ name = "Diana"; city = "Hanoi" };{ name = "Eve"; city = "Paris" };{ name = "Frank"; city = "Hanoi" };{ name = "George"; city = "Paris" }]
let big : (string * Obj.t) list list = (let __groups0 = ref [] in
  List.iter (fun p ->
      let key = Obj.obj (List.assoc "city" p) in
      let cur = try List.assoc key !__groups0 with Not_found -> [] in
      __groups0 := (key, p :: cur) :: List.remove_assoc key !__groups0;
  ) people;
  let __res0 = ref [] in
  List.iter (fun (gKey,gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res0 := { city = g.key; num = List.length g.items } :: !__res0
  ) !__groups0;
  List.rev !__res0)


let () =
  json big;
