open System

module testpkg
let Add a b = a + b
let Pi = 3.14
let Answer = 42

printfn "%A" (testpkg.Add(2, 3))
printfn "%A" (testpkg.Pi)
printfn "%A" (testpkg.Answer)
