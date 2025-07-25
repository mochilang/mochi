// Generated 2025-07-26 04:38 +0700

exception Break
exception Continue

exception Return

type Writer = {
    order: string
    bits: int
    nbits: int
    data: int array
}
type Reader = {
    order: string
    data: int array
    idx: int
    bits: int
    nbits: int
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
and NewReader (data: int array) (order: string) =
    let mutable __ret : Reader = Unchecked.defaultof<Reader>
    let mutable data = data
    let mutable order = order
    try
        __ret <- { order = order; data = data; idx = 0; bits = 0; nbits = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
and readBitsLSB (r: Reader) (width: int) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable r = r
    let mutable width = width
    try
        while (r.nbits) < width do
            if (r.idx) >= (Seq.length (r.data)) then
                __ret <- Map.ofList [("val", box 0); ("eof", box true)]
                raise Return
            let b: int = (r.data).[r.idx]
            r <- { r with idx = (r.idx) + 1 }
            r <- { r with bits = (r.bits) + (int (lshift b (r.nbits))) }
            r <- { r with nbits = (r.nbits) + 8 }
        let mask: int = (int (pow2 width)) - 1
        let out: int = (((r.bits) % (mask + 1) + (mask + 1)) % (mask + 1))
        r <- { r with bits = rshift (r.bits) width }
        r <- { r with nbits = (r.nbits) - width }
        __ret <- Map.ofList [("val", box out); ("eof", box false)]
        raise Return
        __ret
    with
        | Return -> __ret
and readBitsMSB (r: Reader) (width: int) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable r = r
    let mutable width = width
    try
        while (r.nbits) < width do
            if (r.idx) >= (Seq.length (r.data)) then
                __ret <- Map.ofList [("val", box 0); ("eof", box true)]
                raise Return
            let b: int = (r.data).[r.idx]
            r <- { r with idx = (r.idx) + 1 }
            r <- { r with bits = (r.bits) + (int (lshift b (24 - (r.nbits)))) }
            r <- { r with nbits = (r.nbits) + 8 }
        let out: int = rshift (r.bits) (32 - width)
        r <- { r with bits = ((((r.bits) * (int (pow2 width))) % (pow2 32) + (pow2 32)) % (pow2 32)) }
        r <- { r with nbits = (r.nbits) - width }
        __ret <- Map.ofList [("val", box out); ("eof", box false)]
        raise Return
        __ret
    with
        | Return -> __ret
and ReadBits (r: Reader) (width: int) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable r = r
    let mutable width = width
    try
        __ret <- if (r.order) = "LSB" then (readBitsLSB r width) else (readBitsMSB r width)
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
and bytesToHex (bs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable bs = bs
    try
        let digits: string = "0123456789ABCDEF"
        let mutable out: string = ""
        let mutable i: int = 0
        while i < (int (Array.length bs)) do
            let b: int = bs.[i]
            let hi: int = b / 16
            let lo: int = ((b % 16 + 16) % 16)
            out <- (out + (digits.Substring(hi, (hi + 1) - hi))) + (digits.Substring(lo, (lo + 1) - lo))
            if (i + 1) < (int (Array.length bs)) then
                out <- out + " "
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        let mutable idx: int = indexOf upper ch
        if idx >= 0 then
            __ret <- 65 + idx
            raise Return
        idx <- indexOf lower ch
        if idx >= 0 then
            __ret <- 97 + idx
            raise Return
        if (ch >= "0") && (ch <= "9") then
            __ret <- 48 + (int (parseIntStr ch))
            raise Return
        if ch = " " then
            __ret <- 32
            raise Return
        if ch = "." then
            __ret <- 46
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and chr (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        if (n >= 65) && (n < 91) then
            __ret <- upper.Substring(n - 65, (n - 64) - (n - 65))
            raise Return
        if (n >= 97) && (n < 123) then
            __ret <- lower.Substring(n - 97, (n - 96) - (n - 97))
            raise Return
        if (n >= 48) && (n < 58) then
            let digits: string = "0123456789"
            __ret <- digits.Substring(n - 48, (n - 47) - (n - 48))
            raise Return
        if n = 32 then
            __ret <- " "
            raise Return
        if n = 46 then
            __ret <- "."
            raise Return
        __ret <- "?"
        raise Return
        __ret
    with
        | Return -> __ret
and bytesOfStr (s: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable s = s
    try
        let mutable bs: int array = [||]
        let mutable i: int = 0
        while i < (String.length s) do
            bs <- Array.append bs [|ord (s.Substring(i, (i + 1) - i))|]
            i <- i + 1
        __ret <- bs
        raise Return
        __ret
    with
        | Return -> __ret
and bytesToDec (bs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable bs = bs
    try
        let mutable out: string = ""
        let mutable i: int = 0
        while i < (int (Array.length bs)) do
            out <- out + (string (bs.[i]))
            if (i + 1) < (int (Array.length bs)) then
                out <- out + " "
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and Example () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let message: string = "This is a test."
        let msgBytes: int array = bytesOfStr message
        printfn "%s" ((("\"" + message) + "\" as bytes: ") + (unbox<string> (bytesToDec msgBytes)))
        printfn "%s" ("    original bits: " + (unbox<string> (bytesToBits msgBytes)))
        let mutable bw: Writer = NewWriter "MSB"
        let mutable i: int = 0
        while i < (int (Array.length msgBytes)) do
            bw <- WriteBits bw (int (msgBytes.[i])) 7
            i <- i + 1
        bw <- CloseWriter bw
        printfn "%s" ("Written bitstream: " + (unbox<string> (bytesToBits (bw.data))))
        printfn "%s" ("Written bytes: " + (unbox<string> (bytesToHex (bw.data))))
        let mutable br: Reader = NewReader (bw.data) "MSB"
        let mutable result: string = ""
        try
            while true do
                let r: Map<string, obj> = ReadBits br 7
                if unbox<bool> (r.["eof"]) then
                    raise Break
                let v: int = int (r.["val"])
                if v <> 0 then
                    result <- result + (unbox<string> (chr v))
        with
        | Break -> ()
        | Continue -> ()
        printfn "%s" (("Read back as \"" + result) + "\"")
        __ret
    with
        | Return -> __ret
Example()
