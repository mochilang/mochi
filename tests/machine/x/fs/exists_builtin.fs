
let data = [1; 2]
let flag = not (List.isEmpty [ for x in data do if x = 1 then yield x ])
printfn "%A" (flag)
