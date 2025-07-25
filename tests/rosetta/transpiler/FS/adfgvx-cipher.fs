// Generated 2025-07-25 17:17 +0700

exception Return

let mutable _nowSeed:int64 = 0L
let mutable _nowSeeded = false
let _initNow () =
    let s = System.Environment.GetEnvironmentVariable("MOCHI_NOW_SEED")
    if System.String.IsNullOrEmpty(s) |> not then
        match System.Int32.TryParse(s) with
        | true, v ->
            _nowSeed <- int64 v
            _nowSeeded <- true
        | _ -> ()
let _now () =
    if _nowSeeded then
        _nowSeed <- (_nowSeed * 1664525L + 1013904223L) % 2147483647L
        int _nowSeed
    else
        int (System.DateTime.UtcNow.Ticks % 2147483647L)

_initNow()
open System

let mutable adfgvx: string = "ADFGVX"
let mutable alphabet: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
let rec shuffleStr (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable arr: string array = [||]
        let mutable i: int = 0
        while i < (String.length s) do
            arr <- Array.append arr [|s.Substring(i, (i + 1) - i)|]
            i <- i + 1
        let mutable j: int = (Array.length arr) - 1
        while j > 0 do
            let k = (_now()) % (j + 1)
            let tmp: string = arr.[j]
            arr.[j] <- arr.[k]
            arr.[k] <- tmp
            j <- j - 1
        let mutable out: string = ""
        i <- 0
        while i < (Array.length arr) do
            out <- out + (arr.[i])
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and createPolybius () =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    try
        let shuffled: string = shuffleStr alphabet
        let mutable labels: string array = [||]
        let mutable li: int = 0
        while li < (String.length adfgvx) do
            labels <- Array.append labels [|adfgvx.Substring(li, (li + 1) - li)|]
            li <- li + 1
        printfn "%s" "6 x 6 Polybius square:\n"
        printfn "%s" "  | A D F G V X"
        printfn "%s" "---------------"
        let mutable p: string array = [||]
        let mutable i: int = 0
        while i < 6 do
            let mutable row: string = shuffled.Substring(i * 6, ((i + 1) * 6) - (i * 6))
            p <- Array.append p [|row|]
            let mutable line: string = (labels.[i]) + " | "
            let mutable j: int = 0
            while j < 6 do
                line <- (line + (row.Substring(j, (j + 1) - j))) + " "
                j <- j + 1
            printfn "%s" line
            i <- i + 1
        __ret <- p
        raise Return
        __ret
    with
        | Return -> __ret
and createKey (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if (n < 7) || (n > 12) then
            printfn "%s" "Key should be within 7 and 12 letters long."
        let mutable pool: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        let mutable key: string = ""
        let mutable i: int = 0
        while i < n do
            let idx: int = (_now()) % (String.length pool)
            key <- key + (string (pool.[idx]))
            pool <- (pool.Substring(0, idx - 0)) + (pool.Substring(idx + 1, (String.length pool) - (idx + 1)))
            i <- i + 1
        printfn "%s" ("\nThe key is " + key)
        __ret <- key
        raise Return
        __ret
    with
        | Return -> __ret
and orderKey (key: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable key = key
    try
        let mutable pairs = [||]
        let mutable i: int = 0
        while i < (String.length key) do
            pairs <- Array.append pairs [|[|box (key.Substring(i, (i + 1) - i)); box i|]|]
            i <- i + 1
        let mutable n: int = Array.length pairs
        let mutable m: int = 0
        while m < n do
            let mutable j: int = 0
            while j < (n - 1) do
                if (pairs.[j].[0]) > (pairs.[j + 1].[0]) then
                    let tmp = pairs.[j]
                    pairs.[j] <- pairs.[j + 1]
                    pairs.[j + 1] <- tmp
                j <- j + 1
            m <- m + 1
        let mutable res = [||]
        i <- 0
        while i < n do
            res <- Array.append res [|int (pairs.[i].[1])|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and encrypt (polybius: string array) (key: string) (plainText: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable polybius = polybius
    let mutable key = key
    let mutable plainText = plainText
    try
        let mutable labels: string array = [||]
        let mutable li: int = 0
        while li < (String.length adfgvx) do
            labels <- Array.append labels [|adfgvx.Substring(li, (li + 1) - li)|]
            li <- li + 1
        let mutable temp: string = ""
        let mutable i: int = 0
        while i < (String.length plainText) do
            let mutable r: int = 0
            while r < 6 do
                let mutable c: int = 0
                while c < 6 do
                    if (Array.sub polybius.[r] c ((c + 1) - c)) = (plainText.Substring(i, (i + 1) - i)) then
                        temp <- (temp + (Array.sub labels r ((r + 1) - r))) + (Array.sub labels c ((c + 1) - c))
                    c <- c + 1
                r <- r + 1
            i <- i + 1
        let mutable colLen: int = (String.length temp) / (String.length key)
        if ((String.length temp) % (String.length key)) > 0 then
            colLen <- colLen + 1
        let mutable table: string array array = [||]
        let mutable rIdx: int = 0
        while rIdx < colLen do
            let mutable row: string array = [||]
            let mutable j: int = 0
            while j < (String.length key) do
                row <- Array.append row [|""|]
                j <- j + 1
            table <- Array.append table [|row|]
            rIdx <- rIdx + 1
        let mutable idx: int = 0
        while idx < (String.length temp) do
            let row: int = idx / (String.length key)
            let col: int = idx % (String.length key)
            table.[row].[col] <- temp.Substring(idx, (idx + 1) - idx)
            idx <- idx + 1
        let order: int array = orderKey key
        let mutable cols: string array = [||]
        let mutable ci: int = 0
        while ci < (String.length key) do
            let mutable colStr: string = ""
            let mutable ri: int = 0
            while ri < colLen do
                colStr <- colStr + (table.[ri].[order.[ci]])
                ri <- ri + 1
            cols <- Array.append cols [|colStr|]
            ci <- ci + 1
        let mutable result: string = ""
        ci <- 0
        while ci < (Array.length cols) do
            result <- result + (cols.[ci])
            if ci < ((Array.length cols) - 1) then
                result <- result + " "
            ci <- ci + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and indexOf (s: string) (ch: string) =
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
and decrypt (polybius: string array) (key: string) (cipherText: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable polybius = polybius
    let mutable key = key
    let mutable cipherText = cipherText
    try
        let mutable colStrs: string array = [||]
        let mutable start: int = 0
        let mutable i: int = 0
        while i <= (String.length cipherText) do
            if (i = (String.length cipherText)) || ((string (cipherText.[i])) = " ") then
                colStrs <- Array.append colStrs [|cipherText.Substring(start, i - start)|]
                start <- i + 1
            i <- i + 1
        let mutable maxColLen: int = 0
        i <- 0
        while i < (Array.length colStrs) do
            if (Seq.length (colStrs.[i])) > maxColLen then
                maxColLen <- Seq.length (colStrs.[i])
            i <- i + 1
        let mutable cols: string array array = [||]
        i <- 0
        while i < (Array.length colStrs) do
            let mutable s: string = colStrs.[i]
            let mutable ls: string array = [||]
            let mutable j: int = 0
            while j < (String.length s) do
                ls <- Array.append ls [|s.Substring(j, (j + 1) - j)|]
                j <- j + 1
            if (String.length s) < maxColLen then
                let mutable pad: string array = [||]
                let mutable k: int = 0
                while k < maxColLen do
                    if k < (Array.length ls) then
                        pad <- Array.append pad [|ls.[k]|]
                    else
                        pad <- Array.append pad [|""|]
                    k <- k + 1
                cols <- Array.append cols [|pad|]
            else
                cols <- Array.append cols [|ls|]
            i <- i + 1
        let mutable table: string array array = [||]
        let mutable r: int = 0
        while r < maxColLen do
            let mutable row: string array = [||]
            let mutable c: int = 0
            while c < (String.length key) do
                row <- Array.append row [|""|]
                c <- c + 1
            table <- Array.append table [|row|]
            r <- r + 1
        let order: int array = orderKey key
        r <- 0
        while r < maxColLen do
            let mutable c: int = 0
            while c < (String.length key) do
                table.[r].[order.[c]] <- cols.[c].[r]
                c <- c + 1
            r <- r + 1
        let mutable temp: string = ""
        r <- 0
        while r < (Array.length table) do
            let mutable j: int = 0
            while j < (Seq.length (table.[r])) do
                temp <- temp + (table.[r].[j])
                j <- j + 1
            r <- r + 1
        let mutable plainText: string = ""
        let mutable idx: int = 0
        while idx < (String.length temp) do
            let rIdx: int = indexOf adfgvx (temp.Substring(idx, (idx + 1) - idx))
            let cIdx: int = indexOf adfgvx (temp.Substring(idx + 1, (idx + 2) - (idx + 1)))
            plainText <- plainText + (polybius.[rIdx].[cIdx])
            idx <- idx + 2
        __ret <- plainText
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let plainText: string = "ATTACKAT1200AM"
        let polybius: string array = createPolybius()
        let key: string = createKey 9
        printfn "%s" ("\nPlaintext : " + plainText)
        let cipherText: string = encrypt polybius key plainText
        printfn "%s" ("\nEncrypted : " + cipherText)
        let plainText2: string = decrypt polybius key cipherText
        printfn "%s" ("\nDecrypted : " + plainText2)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
