open System

exception Break
exception Continue

let items = [dict [(cat, "a"); (val, 10); (flag, true)]; dict [(cat, "a"); (val, 5); (flag, false)]; dict [(cat, "b"); (val, 20); (flag, true)]]
let result = [ for i in items doyield dict [(cat, g.key); (share, sum [ for x in g doyield (if x.flag then x.val else 0) ] / sum [ for x in g doyield x.val ])] ] |> List.sortBy (fun _ -> g.key)
printfn "%A" (result)
