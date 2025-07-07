let m = dict [("a",1); ("b",2)]
printfn "%b" (m.ContainsKey "a")
printfn "%b" (m.ContainsKey "c")
