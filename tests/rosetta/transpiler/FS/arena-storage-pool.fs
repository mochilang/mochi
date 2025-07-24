// Generated 2025-07-25 00:53 +0700

exception Return

let rec poolPut (p: int array) (x: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable p = p
    let mutable x = x
    try
        __ret <- Array.append p [|x|]
        raise Return
        __ret
    with
        | Return -> __ret
and poolGet (p: int array) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable p = p
    try
        if (Array.length p) = 0 then
            printfn "%s" "pool empty"
            __ret <- Map.ofList [("pool", box p); ("val", box 0)]
            raise Return
        let idx = (Array.length p) - 1
        let v = p.[idx]
        p <- Array.sub p 0 (idx - 0)
        __ret <- Map.ofList [("pool", box p); ("val", box v)]
        raise Return
        __ret
    with
        | Return -> __ret
and clearPool (p: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable p = p
    try
        __ret <- [||]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable pool: int array = [||]
        let mutable i: int = 1
        let mutable j: int = 2
        printfn "%s" (string (i + j))
        pool <- poolPut pool i
        pool <- poolPut pool j
        i <- 0
        j <- 0
        let res1 = poolGet pool
        pool <- (res1.["pool"]) :?> int array
        i <- (res1.["val"]) :?> int
        let res2 = poolGet pool
        pool <- (res2.["pool"]) :?> int array
        j <- (res2.["val"]) :?> int
        i <- 4
        j <- 5
        printfn "%s" (string (i + j))
        pool <- poolPut pool i
        pool <- poolPut pool j
        i <- 0
        j <- 0
        pool <- clearPool pool
        let res3 = poolGet pool
        pool <- (res3.["pool"]) :?> int array
        i <- (res3.["val"]) :?> int
        let res4 = poolGet pool
        pool <- (res4.["pool"]) :?> int array
        j <- (res4.["val"]) :?> int
        i <- 7
        j <- 8
        printfn "%s" (string (i + j))
        __ret
    with
        | Return -> __ret
main()
