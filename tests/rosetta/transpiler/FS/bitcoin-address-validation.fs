// Generated 2025-07-26 05:17 +0700

exception Return

let _sha256 (bs:int array) =
    let sha = System.Security.Cryptography.SHA256.Create()
    let bytes = Array.map byte bs
    let hash = sha.ComputeHash(bytes)
    Array.map int hash

open System.Security.Cryptography

let rec indexOf (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length s) do
            if (s.Substring(i, (i + 1) - i)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and set58 (addr: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable addr = addr
    try
        let tmpl: string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
        let mutable a: int array = [||]
        let mutable i: int = 0
        while i < 25 do
            a <- Array.append a [|0|]
            i <- i + 1
        let mutable idx: int = 0
        while idx < (String.length addr) do
            let ch: string = addr.Substring(idx, (idx + 1) - idx)
            let mutable c: int = indexOf tmpl ch
            if c < 0 then
                __ret <- [||]
                raise Return
            let mutable j: int = 24
            while j >= 0 do
                c <- c + (unbox<int> (58 * (unbox<int> (a.[j]))))
                a.[j] <- ((c % 256 + 256) % 256)
                c <- int (c / 256)
                j <- j - 1
            if c > 0 then
                __ret <- [||]
                raise Return
            idx <- idx + 1
        __ret <- a
        raise Return
        __ret
    with
        | Return -> __ret
and doubleSHA256 (bs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable bs = bs
    try
        let first: int array = _sha256 bs
        __ret <- _sha256 first
        raise Return
        __ret
    with
        | Return -> __ret
and computeChecksum (a: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    try
        let hash: int array = doubleSHA256 (Array.sub a 0 (21 - 0))
        __ret <- Array.sub hash 0 (4 - 0)
        raise Return
        __ret
    with
        | Return -> __ret
and validA58 (addr: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable addr = addr
    try
        let a: int array = set58 addr
        if (unbox<int> (Array.length a)) <> 25 then
            __ret <- false
            raise Return
        if (unbox<int> (a.[0])) <> 0 then
            __ret <- false
            raise Return
        let sum: int array = computeChecksum a
        let mutable i: int = 0
        while i < 4 do
            if (a.[21 + i]) <> (sum.[i]) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (string (validA58 "1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i"))
printfn "%s" (string (validA58 "17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j"))
