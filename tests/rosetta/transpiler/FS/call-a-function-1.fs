// Generated 2025-07-26 04:38 +0700

exception Return

let rec f () =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    try
        __ret <- [|box 0; box 0.0|]
        raise Return
        __ret
    with
        | Return -> __ret
and g (a: int) (b: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and h (s: string) (nums: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable s = s
    let mutable nums = nums
    try

        __ret
    with
        | Return -> __ret
