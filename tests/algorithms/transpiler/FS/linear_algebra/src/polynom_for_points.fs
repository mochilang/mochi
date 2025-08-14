// Generated 2025-08-14 17:48 +0700

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
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let rec _str v =
    match box v with
    | :? float as f -> sprintf "%g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let rec contains_int (xs: int array) (x: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (int i)) = x then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable res: string array = Array.empty<string>
        let mutable current: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = _substring s i (i + 1)
            if ch = sep then
                res <- Array.append res [|current|]
                current <- ""
            else
                current <- current + ch
            i <- i + 1
        res <- Array.append res [|current|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and pow_int_float (``base``: int) (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < exp do
            result <- result * (float ``base``)
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and points_to_polynomial (coordinates: int array array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable coordinates = coordinates
    try
        if (Seq.length (coordinates)) = 0 then
            ignore (failwith ("The program cannot work out a fitting polynomial."))
        let mutable i: int = 0
        while i < (Seq.length (coordinates)) do
            if (Seq.length (_idx coordinates (int i))) <> 2 then
                ignore (failwith ("The program cannot work out a fitting polynomial."))
            i <- i + 1
        let mutable j: int = 0
        while j < (Seq.length (coordinates)) do
            let mutable k: int = j + 1
            while k < (Seq.length (coordinates)) do
                if ((_idx (_idx coordinates (int j)) (int 0)) = (_idx (_idx coordinates (int k)) (int 0))) && ((_idx (_idx coordinates (int j)) (int 1)) = (_idx (_idx coordinates (int k)) (int 1))) then
                    ignore (failwith ("The program cannot work out a fitting polynomial."))
                k <- k + 1
            j <- j + 1
        let mutable set_x: int array = Array.empty<int>
        i <- 0
        while i < (Seq.length (coordinates)) do
            let x_val: int = _idx (_idx coordinates (int i)) (int 0)
            if not (contains_int (set_x) (x_val)) then
                set_x <- Array.append set_x [|x_val|]
            i <- i + 1
        if (Seq.length (set_x)) = 1 then
            __ret <- "x=" + (_str (_idx (_idx coordinates (int 0)) (int 0)))
            raise Return
        if (Seq.length (set_x)) <> (Seq.length (coordinates)) then
            ignore (failwith ("The program cannot work out a fitting polynomial."))
        let n: int = Seq.length (coordinates)
        let mutable matrix: float array array = Array.empty<float array>
        let mutable row: int = 0
        while row < n do
            let mutable line: float array = Array.empty<float>
            let mutable col: int = 0
            while col < n do
                let power: float = pow_int_float (_idx (_idx coordinates (int row)) (int 0)) (n - (col + 1))
                line <- Array.append line [|power|]
                col <- col + 1
            matrix <- Array.append matrix [|line|]
            row <- row + 1
        let mutable vector: float array = Array.empty<float>
        row <- 0
        while row < n do
            vector <- Array.append vector [|(float (_idx (_idx coordinates (int row)) (int 1)))|]
            row <- row + 1
        let mutable count: int = 0
        while count < n do
            let mutable number: int = 0
            while number < n do
                if count <> number then
                    let fraction: float = (_idx (_idx matrix (int number)) (int count)) / (_idx (_idx matrix (int count)) (int count))
                    let mutable cc: int = 0
                    while cc < n do
                        matrix.[number].[cc] <- (_idx (_idx matrix (int number)) (int cc)) - ((_idx (_idx matrix (int count)) (int cc)) * fraction)
                        cc <- cc + 1
                    vector.[number] <- (_idx vector (int number)) - ((_idx vector (int count)) * fraction)
                number <- number + 1
            count <- count + 1
        let mutable solution: string array = Array.empty<string>
        count <- 0
        while count < n do
            let value: float = (_idx vector (int count)) / (_idx (_idx matrix (int count)) (int count))
            solution <- Array.append solution [|(_str (value))|]
            count <- count + 1
        let mutable solved: string = "f(x)="
        count <- 0
        while count < n do
            let mutable parts: string array = split (_idx solution (int count)) ("e")
            let mutable coeff: string = _idx solution (int count)
            if (Seq.length (parts)) > 1 then
                coeff <- ((_idx parts (int 0)) + "*10^") + (_idx parts (int 1))
            solved <- (((solved + "x^") + (_str (n - (count + 1)))) + "*") + coeff
            if (count + 1) <> n then
                solved <- solved + "+"
            count <- count + 1
        __ret <- solved
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (printfn "%s" (points_to_polynomial ([|[|1; 0|]; [|2; 0|]; [|3; 0|]|])))
        ignore (printfn "%s" (points_to_polynomial ([|[|1; 1|]; [|2; 1|]; [|3; 1|]|])))
        ignore (printfn "%s" (points_to_polynomial ([|[|1; 1|]; [|2; 4|]; [|3; 9|]|])))
        ignore (printfn "%s" (points_to_polynomial ([|[|1; 3|]; [|2; 6|]; [|3; 11|]|])))
        ignore (printfn "%s" (points_to_polynomial ([|[|1; -3|]; [|2; -6|]; [|3; -11|]|])))
        ignore (printfn "%s" (points_to_polynomial ([|[|1; 1|]; [|1; 2|]; [|1; 3|]|])))
        ignore (printfn "%s" (points_to_polynomial ([|[|1; 5|]; [|2; 2|]; [|3; 9|]|])))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
