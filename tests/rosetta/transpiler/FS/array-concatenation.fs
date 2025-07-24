// Generated 2025-07-25 01:11 +0700

exception Return

let rec concatInts (a: int array) (b: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        let mutable out: int array = [||]
        for v in a do
            out <- Array.append out [|v|]
        for v in b do
            out <- Array.append out [|v|]
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and concatAny (a: obj array) (b: obj array) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable a = a
    let mutable b = b
    try
        let mutable out: obj array = [||]
        for v in a do
            out <- Array.append out [|v|]
        for v in b do
            out <- Array.append out [|v|]
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let mutable a: int array = [|1; 2; 3|]
let mutable b: int array = [|7; 12; 60|]
printfn "%s" (string (concatInts a b))
let mutable i: obj array = [|1; 2; 3|]
let mutable j: obj array = [|"Crosby"; "Stills"; "Nash"; "Young"|]
printfn "%s" (string (concatAny i j))
let mutable l: int array = [|1; 2; 3|]
let mutable m: int array = [|7; 12; 60|]
printfn "%s" (string (concatInts l m))
