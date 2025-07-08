open System

exception Break
exception Continue

let items = [dict [(cat, "a"); (val, 3)]; dict [(cat, "a"); (val, 1)]; dict [(cat, "b"); (val, 5)]; dict [(cat, "b"); (val, 2)]]
let grouped = [ for i in items doyield dict [(cat, g.key); (total, sum [ for x in g doyield x.val ])] ] |> List.sortByDescending (fun _ -> sum [ for x in g doyield x.val ])
printfn "%A" (grouped)
