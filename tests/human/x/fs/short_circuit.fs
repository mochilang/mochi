let boom a b =
    printfn "boom"
    true
printfn "%b" (false && boom 1 2)
printfn "%b" (true || boom 1 2)
