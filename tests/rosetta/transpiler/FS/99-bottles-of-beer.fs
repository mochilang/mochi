// Generated 2025-07-24 15:22 +0700

exception Return

let rec bottles (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n = 0 then
            __ret <- "No more bottles"
            raise Return
        if n = 1 then
            __ret <- "1 bottle"
            raise Return
        __ret <- (string n) + " bottles"
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let mutable i: int = 99
        while i > 0 do
            printfn "%s" ((bottles i) + " of beer on the wall")
            printfn "%s" ((bottles i) + " of beer")
            printfn "%s" "Take one down, pass it around"
            printfn "%s" ((bottles (i - 1)) + " of beer on the wall")
            i <- i - 1
        __ret
    with
        | Return -> __ret
main()
