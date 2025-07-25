// Generated 2025-07-26 04:38 +0700

exception Break
exception Continue

exception Return

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
        __ret <- "?"
        raise Return
        __ret
    with
        | Return -> __ret
and shiftRune (r: string) (k: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable r = r
    let mutable k = k
    try
        if (r >= "a") && (r <= "z") then
            __ret <- chr (int ((int (((((int ((int (ord r)) - 97)) + k) % 26 + 26) % 26))) + 97))
            raise Return
        if (r >= "A") && (r <= "Z") then
            __ret <- chr (int ((int (((((int ((int (ord r)) - 65)) + k) % 26 + 26) % 26))) + 65))
            raise Return
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and encipher (s: string) (k: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable k = k
    try
        let mutable out: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            out <- out + (unbox<string> (shiftRune (s.Substring(i, (i + 1) - i)) k))
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and decipher (s: string) (k: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable k = k
    try
        __ret <- encipher s ((((26 - (((k % 26 + 26) % 26))) % 26 + 26) % 26))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let pt: string = "The five boxing wizards jump quickly"
        printfn "%s" ("Plaintext: " + pt)
        for key in [|0; 1; 7; 25; 26|] do
            try
                if ((int key) < 1) || ((int key) > 25) then
                    printfn "%s" (("Key " + (string key)) + " invalid")
                    raise Continue
                let ct: string = encipher pt (int key)
                printfn "%s" ("Key " + (string key))
                printfn "%s" ("  Enciphered: " + ct)
                printfn "%s" ("  Deciphered: " + (unbox<string> (decipher ct (int key))))
            with
            | Break -> ()
            | Continue -> ()
        __ret
    with
        | Return -> __ret
main()
