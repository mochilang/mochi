// Generated 2025-08-01 18:27 +0700

exception Return

let rec fd (a: int array) (ord: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable ord = ord
    try
        let mutable i: int = 0
        while i < ord do
            let mutable j: int = 0
            while j < (((Seq.length a) - i) - 1) do
                a.[j] <- (a.[j + 1]) - (a.[j])
                j <- j + 1
            i <- i + 1
        __ret <- Array.sub a 0 (((Seq.length a) - ord) - 0)
        raise Return
        __ret
    with
        | Return -> __ret
let mutable a: int array = [|90; 47; 58; 29; 22; 32; 55; 5; 55; 73|]
printfn "%s" (string a)
printfn "%s" (string (fd a 9))
