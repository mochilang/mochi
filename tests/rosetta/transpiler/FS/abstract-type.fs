// Generated 2025-07-25 13:04 +0700

exception Return

type Beast =
    | Dog of string * string
    | Cat of string * string
let rec beastKind (b: Beast) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable b = b
    try
        __ret <- (match b with
            | Dog(k, _) -> k
            | Cat(k, _) -> k)
        raise Return
        __ret
    with
        | Return -> __ret
and beastName (b: Beast) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable b = b
    try
        __ret <- (match b with
            | Dog(_, n) -> n
            | Cat(_, n) -> n)
        raise Return
        __ret
    with
        | Return -> __ret
and beastCry (b: Beast) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable b = b
    try
        __ret <- (match b with
            | Dog(_, _) -> "Woof"
            | Cat(_, _) -> "Meow")
        raise Return
        __ret
    with
        | Return -> __ret
and bprint (b: Beast) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    try
        printfn "%s" ((((((beastName b) + ", who's a ") + (beastKind b)) + ", cries: \"") + (beastCry b)) + "\".")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let d: Beast = Dog("labrador", "Max")
        let c: Beast = Cat("siamese", "Sammy")
        bprint d
        bprint c
        __ret
    with
        | Return -> __ret
main()
