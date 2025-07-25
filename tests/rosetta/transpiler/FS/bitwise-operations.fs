// Generated 2025-07-26 04:38 +0700

exception Return

let rec toUnsigned16 (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable u: int = n
        if u < 0 then
            u <- u + 65536
        __ret <- ((u % 65536 + 65536) % 65536)
        raise Return
        __ret
    with
        | Return -> __ret
and bin16 (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable u: int = toUnsigned16 n
        let mutable bits: string = ""
        let mutable mask: int = 32768
        for i in 0 .. (16 - 1) do
            if u >= mask then
                bits <- bits + "1"
                u <- u - mask
            else
                bits <- bits + "0"
            mask <- int (mask / 2)
        __ret <- bits
        raise Return
        __ret
    with
        | Return -> __ret
and bit_and (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable ua: int = toUnsigned16 a
        let mutable ub: int = toUnsigned16 b
        let mutable res: int = 0
        let mutable bit: int = 1
        for i in 0 .. (16 - 1) do
            if ((((ua % 2 + 2) % 2)) = 1) && ((((ub % 2 + 2) % 2)) = 1) then
                res <- res + bit
            ua <- int (ua / 2)
            ub <- int (ub / 2)
            bit <- bit * 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and bit_or (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable ua: int = toUnsigned16 a
        let mutable ub: int = toUnsigned16 b
        let mutable res: int = 0
        let mutable bit: int = 1
        for i in 0 .. (16 - 1) do
            if ((((ua % 2 + 2) % 2)) = 1) || ((((ub % 2 + 2) % 2)) = 1) then
                res <- res + bit
            ua <- int (ua / 2)
            ub <- int (ub / 2)
            bit <- bit * 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and bit_xor (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable ua: int = toUnsigned16 a
        let mutable ub: int = toUnsigned16 b
        let mutable res: int = 0
        let mutable bit: int = 1
        for i in 0 .. (16 - 1) do
            let abit: int = ((ua % 2 + 2) % 2)
            let bbit: int = ((ub % 2 + 2) % 2)
            if ((abit = 1) && (bbit = 0)) || ((abit = 0) && (bbit = 1)) then
                res <- res + bit
            ua <- int (ua / 2)
            ub <- int (ub / 2)
            bit <- bit * 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and bit_not (a: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    try
        let mutable ua: int = toUnsigned16 a
        __ret <- 65535 - ua
        raise Return
        __ret
    with
        | Return -> __ret
and shl (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable ua: int = toUnsigned16 a
        let mutable i: int = 0
        while i < b do
            ua <- (((ua * 2) % 65536 + 65536) % 65536)
            i <- i + 1
        __ret <- ua
        raise Return
        __ret
    with
        | Return -> __ret
and shr (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable ua: int = toUnsigned16 a
        let mutable i: int = 0
        while i < b do
            ua <- int (ua / 2)
            i <- i + 1
        __ret <- ua
        raise Return
        __ret
    with
        | Return -> __ret
and las (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- shl a b
        raise Return
        __ret
    with
        | Return -> __ret
and ras (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable ``val``: int = a
        let mutable i: int = 0
        while i < b do
            if ``val`` >= 0 then
                ``val`` <- int (``val`` / 2)
            else
                ``val`` <- int ((``val`` - 1) / 2)
            i <- i + 1
        __ret <- toUnsigned16 ``val``
        raise Return
        __ret
    with
        | Return -> __ret
and rol (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable ua: int = toUnsigned16 a
        let left: int = shl ua b
        let right: int = shr ua (16 - b)
        __ret <- toUnsigned16 (left + right)
        raise Return
        __ret
    with
        | Return -> __ret
and ror (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable ua: int = toUnsigned16 a
        let right: int = shr ua b
        let left: int = shl ua (16 - b)
        __ret <- toUnsigned16 (left + right)
        raise Return
        __ret
    with
        | Return -> __ret
and bitwise (a: int) (b: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable a = a
    let mutable b = b
    try
        printfn "%s" ("a:   " + (unbox<string> (bin16 a)))
        printfn "%s" ("b:   " + (unbox<string> (bin16 b)))
        printfn "%s" ("and: " + (unbox<string> (bin16 (int (bit_and a b)))))
        printfn "%s" ("or:  " + (unbox<string> (bin16 (int (bit_or a b)))))
        printfn "%s" ("xor: " + (unbox<string> (bin16 (int (bit_xor a b)))))
        printfn "%s" ("not: " + (unbox<string> (bin16 (int (bit_not a)))))
        if b < 0 then
            printfn "%s" "Right operand is negative, but all shifts require an unsigned right operand (shift distance)."
            __ret <- null
            raise Return
        printfn "%s" ("shl: " + (unbox<string> (bin16 (int (shl a b)))))
        printfn "%s" ("shr: " + (unbox<string> (bin16 (int (shr a b)))))
        printfn "%s" ("las: " + (unbox<string> (bin16 (int (las a b)))))
        printfn "%s" ("ras: " + (unbox<string> (bin16 (int (ras a b)))))
        printfn "%s" ("rol: " + (unbox<string> (bin16 (int (rol a b)))))
        printfn "%s" ("ror: " + (unbox<string> (bin16 (int (ror a b)))))
        __ret
    with
        | Return -> __ret
bitwise (-460) 6
