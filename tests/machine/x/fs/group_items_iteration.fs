open System

exception Break
exception Continue

let data = [dict [(tag, "a"); (val, 1)]; dict [(tag, "a"); (val, 2)]; dict [(tag, "b"); (val, 3)]]
let groups = [ for d in data doyield g ]
let mutable tmp = []
try
    for g in groups do
        try
            let mutable total = 0
            try
                for x in g.items do
                    try
                        total <- total + x.val
                    with Continue -> ()
            with Break -> ()
            tmp <- tmp @ [dict [(tag, g.key); (total, total)]]
        with Continue -> ()
with Break -> ()
let result = [ for r in tmp doyield r ] |> List.sortBy (fun _ -> r.tag)
printfn "%A" (result)
