// Generated 2025-07-26 04:38 +0700

exception Return

type Writer = {
    order: string
    bits: int
    nbits: int
    data: int array
}
let rec pow2 (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable v: int = 1
        let mutable i: int = 0
        while i < n do
            v <- v * 2
            i <- i + 1
        __ret <- v
        raise Return
        __ret
    with
        | Return -> __ret
and lshift (x: int) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    let mutable n = n
    try
        __ret <- x * (int (pow2 n))
        raise Return
        __ret
    with
        | Return -> __ret
and rshift (x: int) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    let mutable n = n
    try
        __ret <- x / (int (pow2 n))
        raise Return
        __ret
    with
        | Return -> __ret
and NewWriter (order: string) =
    let mutable __ret : Writer = Unchecked.defaultof<Writer>
    let mutable order = order
    try
        __ret <- { order = order; bits = 0; nbits = 0; data = [||] }
        raise Return
        __ret
    with
        | Return -> __ret
and writeBitsLSB (w: Writer) (c: int) (width: int) =
    let mutable __ret : Writer = Unchecked.defaultof<Writer>
    let mutable w = w
    let mutable c = c
    let mutable width = width
    try
        w <- { w with bits = (w.bits) + (int (lshift c (w.nbits))) }
        w <- { w with nbits = (w.nbits) + width }
        while (w.nbits) >= 8 do
            let b: int = (((w.bits) % 256 + 256) % 256)
            w <- { w with data = Array.append (w.data) [|b|] }
            w <- { w with bits = rshift (w.bits) 8 }
            w <- { w with nbits = (w.nbits) - 8 }
        __ret <- w
        raise Return
        __ret
    with
        | Return -> __ret
and writeBitsMSB (w: Writer) (c: int) (width: int) =
    let mutable __ret : Writer = Unchecked.defaultof<Writer>
    let mutable w = w
    let mutable c = c
    let mutable width = width
    try
        w <- { w with bits = (w.bits) + (int (lshift c ((32 - width) - (w.nbits)))) }
        w <- { w with nbits = (w.nbits) + width }
        while (w.nbits) >= 8 do
            let b: int = (((rshift (w.bits) 24) % 256 + 256) % 256)
            w <- { w with data = Array.append (w.data) [|b|] }
            w <- { w with bits = (int ((((w.bits) % (pow2 24) + (pow2 24)) % (pow2 24)))) * 256 }
            w <- { w with nbits = (w.nbits) - 8 }
        __ret <- w
        raise Return
        __ret
    with
        | Return -> __ret
and WriteBits (w: Writer) (c: int) (width: int) =
    let mutable __ret : Writer = Unchecked.defaultof<Writer>
    let mutable w = w
    let mutable c = c
    let mutable width = width
    try
        __ret <- if (w.order) = "LSB" then (writeBitsLSB w c width) else (writeBitsMSB w c width)
        raise Return
        __ret
    with
        | Return -> __ret
and CloseWriter (w: Writer) =
    let mutable __ret : Writer = Unchecked.defaultof<Writer>
    let mutable w = w
    try
        if (w.nbits) > 0 then
            if (w.order) = "MSB" then
                w <- { w with bits = rshift (w.bits) 24 }
            w <- { w with data = Array.append (w.data) [|(((w.bits) % 256 + 256) % 256)|] }
        w <- { w with bits = 0 }
        w <- { w with nbits = 0 }
        __ret <- w
        raise Return
        __ret
    with
        | Return -> __ret
and toBinary (n: int) (bits: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable bits = bits
    try
        let mutable b: string = ""
        let mutable ``val``: int = n
        let mutable i: int = 0
        while i < bits do
            b <- (string (((``val`` % 2 + 2) % 2))) + b
            ``val`` <- ``val`` / 2
            i <- i + 1
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
and bytesToBits (bs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable bs = bs
    try
        let mutable out: string = "["
        let mutable i: int = 0
        while i < (int (Array.length bs)) do
            out <- out + (unbox<string> (toBinary (int (bs.[i])) 8))
            if (i + 1) < (int (Array.length bs)) then
                out <- out + " "
            i <- i + 1
        out <- out + "]"
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and ExampleWriter_WriteBits () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable bw: Writer = NewWriter "MSB"
        bw <- WriteBits bw 15 4
        bw <- WriteBits bw 0 1
        bw <- WriteBits bw 19 5
        bw <- CloseWriter bw
        printfn "%A" (bytesToBits (bw.data))
        __ret
    with
        | Return -> __ret
ExampleWriter_WriteBits()
