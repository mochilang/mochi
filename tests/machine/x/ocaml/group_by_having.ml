type ('k,'v) group = { key : 'k; items : 'v list }

let people = [[("name",Obj.repr ("Alice"));("city",Obj.repr ("Paris"))];[("name",Obj.repr ("Bob"));("city",Obj.repr ("Hanoi"))];[("name",Obj.repr ("Charlie"));("city",Obj.repr ("Paris"))];[("name",Obj.repr ("Diana"));("city",Obj.repr ("Hanoi"))];[("name",Obj.repr ("Eve"));("city",Obj.repr ("Paris"))];[("name",Obj.repr ("Frank"));("city",Obj.repr ("Hanoi"))];[("name",Obj.repr ("George"));("city",Obj.repr ("Paris"))]]
let big = (let __groups0 = ref [] in
  List.iter (fun p ->
      let key = Obj.obj (List.assoc "city" p) in
      let cur = try List.assoc key !__groups0 with Not_found -> [] in
      __groups0 := (key, p :: cur) :: List.remove_assoc key !__groups0;
  ) people;
  let __res0 = ref [] in
  List.iter (fun (gKey,gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res0 := [("city",Obj.repr (g.key));("num",Obj.repr (List.length g.items))] :: !__res0
  ) !__groups0;
  List.rev !__res0)


let () =
  json big;
