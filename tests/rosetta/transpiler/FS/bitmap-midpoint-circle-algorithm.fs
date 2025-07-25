// Generated 2025-07-26 04:38 +0700

exception Return

let rec initGrid (size: int) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable size = size
    try
        let mutable g: string array array = [||]
        let mutable y: int = 0
        while y < size do
            let mutable row: string array = [||]
            let mutable x: int = 0
            while x < size do
                row <- Array.append row [|" "|]
                x <- x + 1
            g <- Array.append g [|row|]
            y <- y + 1
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
and set (g: string array array) (x: int) (y: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable x = x
    let mutable y = y
    try
        if (((x >= 0) && (x < (Seq.length (g.[0])))) && (y >= 0)) && (y < (int (Array.length g))) then
            (g.[y]).[x] <- "#"
        __ret
    with
        | Return -> __ret
and circle (r: int) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable r = r
    try
        let size: int = (r * 2) + 1
        let mutable g: string array array = initGrid size
        let mutable x: int = r
        let mutable y: int = 0
        let mutable err: int = 1 - r
        while y <= x do
            set g (r + x) (r + y)
            set g (r + y) (r + x)
            set g (r - x) (r + y)
            set g (r - y) (r + x)
            set g (r - x) (r - y)
            set g (r - y) (r - x)
            set g (r + x) (r - y)
            set g (r + y) (r - x)
            y <- y + 1
            if err < 0 then
                err <- (err + (2 * y)) + 1
            else
                x <- x - 1
                err <- (err + (2 * (y - x))) + 1
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
and trimRight (row: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable row = row
    try
        let mutable ``end``: int = Array.length row
        while (``end`` > 0) && ((unbox<string> (row.[``end`` - 1])) = " ") do
            ``end`` <- ``end`` - 1
        let mutable s: string = ""
        let mutable i: int = 0
        while i < ``end`` do
            s <- s + (unbox<string> (row.[i]))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let mutable g: string array array = circle 10
for row in g do
    printfn "%A" (trimRight (unbox<string array> row))
