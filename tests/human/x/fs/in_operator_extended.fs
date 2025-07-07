let xs = [1;2;3]
let ys = xs |> List.filter (fun x -> x % 2 = 1)
printfn "%b" (List.contains 1 ys)
printfn "%b" (List.contains 2 ys)

let m = dict [("a",1)]
printfn "%b" (m.ContainsKey "a")
printfn "%b" (m.ContainsKey "b")

let s = "hello"
printfn "%b" (s.Contains "ell")
printfn "%b" (s.Contains "foo")
