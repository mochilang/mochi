open System

exception Break
exception Continue

let nations = [dict [(id, 1); (name, "A")]; dict [(id, 2); (name, "B")]]
let suppliers = [dict [(id, 1); (nation, 1)]; dict [(id, 2); (nation, 2)]]
let partsupp = [dict [(part, 100); (supplier, 1); (cost, 10); (qty, 2)]; dict [(part, 100); (supplier, 2); (cost, 20); (qty, 1)]; dict [(part, 200); (supplier, 1); (cost, 5); (qty, 3)]]
let filtered = [ for ps in partsupp do if n.name = "A" then yield dict [(part, ps.part); (value, ps.cost * ps.qty)] ]
let grouped = [ for x in filtered doyield dict [(part, g.key); (total, sum [ for r in g doyield r.value ])] ]
printfn "%A" (grouped)
