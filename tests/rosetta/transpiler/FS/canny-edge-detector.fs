// Generated 2025-07-27 22:40 +0700

exception Return

let PI: float = 3.141592653589793
let rec conv2d (img: float array array) (k: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable img = img
    let mutable k = k
    try
        let h: int = Array.length img
        let w: int = Seq.length (img.[0])
        let n: int = Array.length k
        let half: int = n / 2
        let mutable out: float array array = [||]
        let mutable y: int = 0
        while y < h do
            let mutable row: float array = [||]
            let mutable x: int = 0
            while x < w do
                let mutable sum: float = 0.0
                let mutable j: int = 0
                while j < n do
                    let mutable i: int = 0
                    while i < n do
                        let mutable yy: int = (y + j) - half
                        if yy < 0 then
                            yy <- 0
                        if yy >= h then
                            yy <- h - 1
                        let mutable xx: int = (x + i) - half
                        if xx < 0 then
                            xx <- 0
                        if xx >= w then
                            xx <- w - 1
                        sum <- unbox<float> (sum + (unbox<float> (((img.[yy]).[xx]) * ((k.[j]).[i]))))
                        i <- i + 1
                    j <- j + 1
                row <- unbox<float array> (Array.append row [|sum|])
                x <- x + 1
            out <- unbox<float array array> (Array.append out [|row|])
            y <- y + 1
        __ret <- unbox<float array array> out
        raise Return
        __ret
    with
        | Return -> __ret
and gradient (img: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable img = img
    try
        let hx: float array array = [|[|-1.0; 0.0; 1.0|]; [|-2.0; 0.0; 2.0|]; [|-1.0; 0.0; 1.0|]|]
        let hy: float array array = [|[|1.0; 2.0; 1.0|]; [|0.0; 0.0; 0.0|]; [|-1.0; -2.0; -1.0|]|]
        let mutable gx: float array array = conv2d img hx
        let mutable gy: float array array = conv2d img hy
        let mutable h: int = Array.length img
        let mutable w: int = Seq.length (img.[0])
        let mutable out: float array array = [||]
        let mutable y: int = 0
        while y < h do
            let mutable row: float array = [||]
            let mutable x: int = 0
            while x < w do
                let g = (((gx.[y]).[x]) * ((gx.[y]).[x])) + (((gy.[y]).[x]) * ((gy.[y]).[x]))
                row <- unbox<float array> (Array.append row [|g|])
                x <- x + 1
            out <- unbox<float array array> (Array.append out [|row|])
            y <- y + 1
        __ret <- unbox<float array array> out
        raise Return
        __ret
    with
        | Return -> __ret
and threshold (g: float array array) (t: float) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable g = g
    let mutable t = t
    try
        let mutable h: int = Array.length g
        let mutable w: int = Seq.length (g.[0])
        let mutable out: int array array = [||]
        let mutable y: int = 0
        while y < h do
            let mutable row: int array = [||]
            let mutable x: int = 0
            while x < w do
                if (unbox<float> ((g.[y]).[x])) >= t then
                    row <- unbox<int array> (Array.append row [|1|])
                else
                    row <- unbox<int array> (Array.append row [|0|])
                x <- x + 1
            out <- unbox<int array array> (Array.append out [|row|])
            y <- y + 1
        __ret <- unbox<int array array> out
        raise Return
        __ret
    with
        | Return -> __ret
and printMatrix (m: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable m = m
    try
        let mutable y: int = 0
        while y < (unbox<int> (Array.length m)) do
            let mutable line: string = ""
            let mutable x: int = 0
            while x < (Seq.length (m.[0])) do
                line <- line + (string ((m.[y]).[x]))
                if x < ((Seq.length (m.[0])) - 1) then
                    line <- line + " "
                x <- x + 1
            printfn "%s" line
            y <- y + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let img: float array array = [|[|0.0; 0.0; 0.0; 0.0; 0.0|]; [|0.0; 255.0; 255.0; 255.0; 0.0|]; [|0.0; 255.0; 255.0; 255.0; 0.0|]; [|0.0; 255.0; 255.0; 255.0; 0.0|]; [|0.0; 0.0; 0.0; 0.0; 0.0|]|]
        let g: float array array = gradient img
        let edges: int array array = threshold g (1020.0 * 1020.0)
        printMatrix edges
        __ret
    with
        | Return -> __ret
main()
