// Generated 2025-07-26 05:05 +0700

exception Return

let rec each (xs: int array) (f: int -> unit) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable xs = xs
    let mutable f = f
    try
        for x in xs do
            f x
        __ret
    with
        | Return -> __ret
and Map (xs: int array) (f: int -> int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable f = f
    try
        let mutable r: int array = [||]
        for x in xs do
            r <- Array.append r [|f x|]
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let s: int array = [|1; 2; 3; 4; 5|]
        each s (unbox<int -> unit> (        fun i -> (printfn "%s" (string (i * i)))))
        printfn "%s" (string (Map s (unbox<int -> int> (        fun i -> (i * i)))))
        __ret
    with
        | Return -> __ret
main()
