// Generated 2025-07-24 08:38 +0000

open System

let rec main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let a = int (System.Console.ReadLine())
        let b = int (System.Console.ReadLine())
        printfn "%A" (a + b)
        __ret
    with
        | Return -> __ret
main()
