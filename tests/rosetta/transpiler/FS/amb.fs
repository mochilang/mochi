// Generated 2025-07-25 14:38 +0000

exception Return

let rec amb (wordsets: string array array) (res: string array) (idx: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable wordsets = wordsets
    let mutable res = res
    let mutable idx = idx
    try
        if idx = (unbox<int> (Array.length wordsets)) then
            __ret <- true
            raise Return
        let mutable prev: string = ""
        if idx > 0 then
            prev <- res.[idx - 1]
        let mutable i: int = 0
        while i < (Seq.length (wordsets.[idx])) do
            let w = wordsets.[idx].[i]
            if (idx = 0) || ((prev.Substring((String.length prev) - 1, (String.length prev) - ((String.length prev) - 1))) = (w.Substring(0, 1 - 0))) then
                res.[idx] <- w
                if amb wordsets res (idx + 1) then
                    __ret <- true
                    raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let wordset: string array array = [|[|"the"; "that"; "a"|]; [|"frog"; "elephant"; "thing"|]; [|"walked"; "treaded"; "grows"|]; [|"slowly"; "quickly"|]|]
        let mutable res: string array = [||]
        let mutable i: int = 0
        while i < (unbox<int> (Array.length wordset)) do
            res <- Array.append res [|""|]
            i <- i + 1
        if amb wordset res 0 then
            let mutable out: string = "[" + (unbox<string> (res.[0]))
            let mutable j: int = 1
            while j < (unbox<int> (Array.length res)) do
                out <- (out + " ") + (unbox<string> (res.[j]))
                j <- j + 1
            out <- out + "]"
            printfn "%s" out
        else
            printfn "%s" "No amb found"
        __ret
    with
        | Return -> __ret
main()
