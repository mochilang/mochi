// Generated 2025-07-24 20:52 +0700

exception Return

let dim: int = 16
let rec newPile (d: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable d = d
    try
        let mutable b: int array array = [||]
        let mutable y: int = 0
        while y < d do
            let mutable row: int array = [||]
            let mutable x: int = 0
            while x < d do
                row <- Array.append row [|0|]
                x <- x + 1
            b <- Array.append b [|row|]
            y <- y + 1
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
and handlePile (pile: int array array) (x: int) (y: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable pile = pile
    let mutable x = x
    let mutable y = y
    try
        if (pile.[y].[x]) >= 4 then
            pile.[y].[x] <- (pile.[y].[x]) - 4
            if y > 0 then
                pile.[y - 1].[x] <- (pile.[y - 1].[x]) + 1
                if (pile.[y - 1].[x]) >= 4 then
                    pile <- handlePile pile x (y - 1)
            if x > 0 then
                pile.[y].[x - 1] <- (pile.[y].[x - 1]) + 1
                if (pile.[y].[x - 1]) >= 4 then
                    pile <- handlePile pile (x - 1) y
            if y < (dim - 1) then
                pile.[y + 1].[x] <- (pile.[y + 1].[x]) + 1
                if (pile.[y + 1].[x]) >= 4 then
                    pile <- handlePile pile x (y + 1)
            if x < (dim - 1) then
                pile.[y].[x + 1] <- (pile.[y].[x + 1]) + 1
                if (pile.[y].[x + 1]) >= 4 then
                    pile <- handlePile pile (x + 1) y
            pile <- handlePile pile x y
        __ret <- pile
        raise Return
        __ret
    with
        | Return -> __ret
and drawPile (pile: int array array) (d: int) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable pile = pile
    let mutable d = d
    try
        let chars: string array = [|" "; "░"; "▓"; "█"|]
        let mutable row: int = 0
        while row < d do
            let mutable line: string = ""
            let mutable col: int = 0
            while col < d do
                let mutable v = pile.[row].[col]
                if v > 3 then
                    v <- 3
                line <- line + (chars.[v])
                col <- col + 1
            printfn "%s" line
            row <- row + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let mutable pile = newPile 16
        let hdim: int = 7
        pile.[hdim].[hdim] <- 16
        pile <- handlePile pile hdim hdim
        drawPile pile 16
        __ret
    with
        | Return -> __ret
main()
