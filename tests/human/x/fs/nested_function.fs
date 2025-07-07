let outer x =
    let inner y = x + y
    inner 5
printfn "%d" (outer 3)
