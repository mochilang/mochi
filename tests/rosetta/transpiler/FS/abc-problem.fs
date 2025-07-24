// Generated 2025-07-24 20:52 +0700

exception Return

let rec fields (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable res: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            let c: string = s.Substring(i, (i + 1) - i)
            if c = " " then
                if (String.length cur) > 0 then
                    res <- Array.append res [|cur|]
                    cur <- ""
            else
                cur <- cur + c
            i <- i + 1
        if (String.length cur) > 0 then
            res <- Array.append res [|cur|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and canSpell (word: string) (blks: string array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable word = word
    let mutable blks = blks
    try
        if (String.length word) = 0 then
            __ret <- true
            raise Return
        let c = word.Substring(0, 1 - 0).ToLower()
        let mutable i: int = 0
        while i < (Seq.length blks) do
            let b = blks.[i]
            if (c = (Array.sub b 0 (1 - 0).ToLower())) || (c = (Array.sub b 1 (2 - 1).ToLower())) then
                let mutable rest: string array = [||]
                let mutable j: int = 0
                while j < (Seq.length blks) do
                    if j <> i then
                        rest <- Array.append rest [|blks.[j]|]
                    j <- j + 1
                if canSpell (word.Substring(1, (String.length word) - 1)) rest then
                    __ret <- true
                    raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and newSpeller (blocks: string) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable blocks = blocks
    try
        let bl = fields blocks
        __ret <-         fun w -> (canSpell w bl)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let sp = newSpeller "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM"
        for word in [|"A"; "BARK"; "BOOK"; "TREAT"; "COMMON"; "SQUAD"; "CONFUSE"|] do
            printfn "%s" ((word + " ") + (string (sp word)))
        __ret
    with
        | Return -> __ret
main()
