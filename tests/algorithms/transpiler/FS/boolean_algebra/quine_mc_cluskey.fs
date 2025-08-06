// Generated 2025-08-06 22:14 +0700

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec compare_string (string1: string) (string2: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable string1 = string1
    let mutable string2 = string2
    try
        let mutable result: string = ""
        let mutable count: int = 0
        let mutable i: int = 0
        while i < (String.length (string1)) do
            let c1: string = _substring string1 i (i + 1)
            let c2: string = _substring string2 i (i + 1)
            if c1 <> c2 then
                count <- count + 1
                result <- result + "_"
            else
                result <- result + c1
            i <- i + 1
        if count > 1 then
            __ret <- ""
            raise Return
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and contains_string (arr: string array) (value: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable arr = arr
    let mutable value = value
    try
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            if (_idx arr (i)) = value then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and unique_strings (arr: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable arr = arr
    try
        let mutable res: string array = [||]
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            if not (contains_string (res) (_idx arr (i))) then
                res <- Array.append res [|_idx arr (i)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and check (binary: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable binary = binary
    try
        let mutable pi: string array = [||]
        let mutable current: string array = binary
        while true do
            let mutable check1: string array = [||]
            let mutable i: int = 0
            while i < (Seq.length (current)) do
                check1 <- Array.append check1 [|"$"|]
                i <- i + 1
            let mutable temp: string array = [||]
            i <- 0
            while i < (Seq.length (current)) do
                let mutable j: int = i + 1
                while j < (Seq.length (current)) do
                    let mutable k: string = compare_string (_idx current (i)) (_idx current (j))
                    if k = "" then
                        check1 <- _arrset check1 i "*"
                        check1 <- _arrset check1 j "*"
                        temp <- Array.append temp [|"X"|]
                    j <- j + 1
                i <- i + 1
            i <- 0
            while i < (Seq.length (current)) do
                if (_idx check1 (i)) = "$" then
                    pi <- Array.append pi [|_idx current (i)|]
                i <- i + 1
            if (Seq.length (temp)) = 0 then
                __ret <- pi
                raise Return
            current <- unique_strings (temp)
        __ret
    with
        | Return -> __ret
and decimal_to_binary (no_of_variable: int) (minterms: int array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable no_of_variable = no_of_variable
    let mutable minterms = minterms
    try
        let mutable temp: string array = [||]
        let mutable idx: int = 0
        while idx < (Seq.length (minterms)) do
            let mutable minterm: int = _idx minterms (idx)
            let mutable string: string = ""
            let mutable i: int = 0
            while i < no_of_variable do
                string <- (_str (((minterm % 2 + 2) % 2))) + string
                minterm <- minterm / 2
                i <- i + 1
            temp <- Array.append temp [|string|]
            idx <- idx + 1
        __ret <- temp
        raise Return
        __ret
    with
        | Return -> __ret
and is_for_table (string1: string) (string2: string) (count: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable string1 = string1
    let mutable string2 = string2
    let mutable count = count
    try
        let mutable count_n: int = 0
        let mutable i: int = 0
        while i < (String.length (string1)) do
            let c1: string = _substring string1 i (i + 1)
            let c2: string = _substring string2 i (i + 1)
            if c1 <> c2 then
                count_n <- count_n + 1
            i <- i + 1
        __ret <- count_n = count
        raise Return
        __ret
    with
        | Return -> __ret
and count_ones (row: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable row = row
    try
        let mutable c: int = 0
        let mutable j: int = 0
        while j < (Seq.length (row)) do
            if (_idx row (j)) = 1 then
                c <- c + 1
            j <- j + 1
        __ret <- c
        raise Return
        __ret
    with
        | Return -> __ret
and selection (chart: int array array) (prime_implicants: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable chart = chart
    let mutable prime_implicants = prime_implicants
    try
        let mutable temp: string array = [||]
        let mutable select: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (chart)) do
            select <- Array.append select [|0|]
            i <- i + 1
        let mutable col: int = 0
        while col < (Seq.length (_idx chart (0))) do
            let mutable count: int = 0
            let mutable row: int = 0
            while row < (Seq.length (chart)) do
                if (_idx (_idx chart (row)) (col)) = 1 then
                    count <- count + 1
                row <- row + 1
            if count = 1 then
                let mutable rem: int = 0
                row <- 0
                while row < (Seq.length (chart)) do
                    if (_idx (_idx chart (row)) (col)) = 1 then
                        rem <- row
                    row <- row + 1
                select <- _arrset select rem 1
            col <- col + 1
        i <- 0
        while i < (Seq.length (select)) do
            if (_idx select (i)) = 1 then
                let mutable j: int = 0
                while j < (Seq.length (_idx chart (0))) do
                    if (_idx (_idx chart (i)) (j)) = 1 then
                        let mutable r: int = 0
                        while r < (Seq.length (chart)) do
                            chart.[r].[j] <- 0
                            r <- r + 1
                    j <- j + 1
                temp <- Array.append temp [|_idx prime_implicants (i)|]
            i <- i + 1
        while true do
            let mutable counts: int array = [||]
            let mutable r: int = 0
            while r < (Seq.length (chart)) do
                counts <- Array.append counts [|count_ones (_idx chart (r))|]
                r <- r + 1
            let mutable max_n: int = _idx counts (0)
            let mutable rem: int = 0
            let mutable k: int = 1
            while k < (Seq.length (counts)) do
                if (_idx counts (k)) > max_n then
                    max_n <- _idx counts (k)
                    rem <- k
                k <- k + 1
            if max_n = 0 then
                __ret <- temp
                raise Return
            temp <- Array.append temp [|_idx prime_implicants (rem)|]
            let mutable j: int = 0
            while j < (Seq.length (_idx chart (0))) do
                if (_idx (_idx chart (rem)) (j)) = 1 then
                    let mutable r2: int = 0
                    while r2 < (Seq.length (chart)) do
                        chart.[r2].[j] <- 0
                        r2 <- r2 + 1
                j <- j + 1
        __ret
    with
        | Return -> __ret
and count_char (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable cnt: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (_substring s i (i + 1)) = ch then
                cnt <- cnt + 1
            i <- i + 1
        __ret <- cnt
        raise Return
        __ret
    with
        | Return -> __ret
and prime_implicant_chart (prime_implicants: string array) (binary: string array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable prime_implicants = prime_implicants
    let mutable binary = binary
    try
        let mutable chart: int array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (prime_implicants)) do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < (Seq.length (binary)) do
                row <- Array.append row [|0|]
                j <- j + 1
            chart <- Array.append chart [|row|]
            i <- i + 1
        i <- 0
        while i < (Seq.length (prime_implicants)) do
            let mutable count: int = count_char (_idx prime_implicants (i)) ("_")
            let mutable j: int = 0
            while j < (Seq.length (binary)) do
                if is_for_table (_idx prime_implicants (i)) (_idx binary (j)) (count) then
                    chart.[i].[j] <- 1
                j <- j + 1
            i <- i + 1
        __ret <- chart
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let no_of_variable: int = 3
        let minterms: int array = [|1; 5; 7|]
        let binary: string array = decimal_to_binary (no_of_variable) (minterms)
        let prime_implicants: string array = check (binary)
        printfn "%s" ("Prime Implicants are:")
        printfn "%s" (_str (prime_implicants))
        let mutable chart: int array array = prime_implicant_chart (prime_implicants) (binary)
        let essential_prime_implicants: string array = selection (chart) (prime_implicants)
        printfn "%s" ("Essential Prime Implicants are:")
        printfn "%s" (_str (essential_prime_implicants))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
