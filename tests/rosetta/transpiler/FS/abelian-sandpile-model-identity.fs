// Generated 2025-07-25 13:04 +0700

exception Return

let rec neighborsList () =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    try
        __ret <- [|[|1; 3|]; [|0; 2; 4|]; [|1; 5|]; [|0; 4; 6|]; [|1; 3; 5; 7|]; [|2; 4; 8|]; [|3; 7|]; [|4; 6; 8|]; [|5; 7|]|]
        raise Return
        __ret
    with
        | Return -> __ret
and plus (a: int array) (b: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < (Array.length a) do
            res <- Array.append res [|(a.[i]) + (b.[i])|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and isStable (p: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable p = p
    try
        for v in p do
            if v > 3 then
                __ret <- false
                raise Return
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and topple (p: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable p = p
    try
        let neighbors = neighborsList()
        let mutable i: int = 0
        while i < (Array.length p) do
            if (p.[i]) > 3 then
                p.[i] <- (p.[i]) - 4
                let nbs = neighbors.[i]
                for j in nbs do
                    p.[j] <- (p.[j]) + 1
                __ret <- 0
                raise Return
            i <- i + 1
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and pileString (p: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable p = p
    try
        let mutable s: string = ""
        let mutable r: int = 0
        while r < 3 do
            let mutable c: int = 0
            while c < 3 do
                s <- (s + (string (p.[(3 * r) + c]))) + " "
                c <- c + 1
            s <- s + "\n"
            r <- r + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" "Avalanche of topplings:\n"
let mutable s4: int array = [|4; 3; 3; 3; 1; 2; 0; 2; 3|]
printfn "%A" (pileString s4)
while not (isStable s4) do
    topple s4
    printfn "%A" (pileString s4)
printfn "%s" "Commutative additions:\n"
let mutable s1: int array = [|1; 2; 0; 2; 1; 1; 0; 1; 3|]
let mutable s2: int array = [|2; 1; 3; 1; 0; 1; 0; 1; 0|]
let mutable s3_a = plus s1 s2
while not (isStable s3_a) do
    topple s3_a
let mutable s3_b = plus s2 s1
while not (isStable s3_b) do
    topple s3_b
printfn "%s" (((((pileString s1) + "\nplus\n\n") + (pileString s2)) + "\nequals\n\n") + (pileString s3_a))
printfn "%s" ((((("and\n\n" + (pileString s2)) + "\nplus\n\n") + (pileString s1)) + "\nalso equals\n\n") + (pileString s3_b))
printfn "%s" "Addition of identity sandpile:\n"
let mutable s3: int array = [|3; 3; 3; 3; 3; 3; 3; 3; 3|]
let mutable s3_id: int array = [|2; 1; 2; 1; 0; 1; 2; 1; 2|]
let mutable s4b = plus s3 s3_id
while not (isStable s4b) do
    topple s4b
printfn "%s" (((((pileString s3) + "\nplus\n\n") + (pileString s3_id)) + "\nequals\n\n") + (pileString s4b))
printfn "%s" "Addition of identities:\n"
let mutable s5 = plus s3_id s3_id
while not (isStable s5) do
    topple s5
printfn "%s" (((((pileString s3_id) + "\nplus\n\n") + (pileString s3_id)) + "\nequals\n\n") + (pileString s5))
