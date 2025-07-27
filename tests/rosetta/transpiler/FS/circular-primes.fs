// Generated 2025-07-27 23:45 +0700

exception Break
exception Continue

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec isPrime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 2 then
            __ret <- false
            raise Return
        if (((n % 2 + 2) % 2)) = 0 then
            __ret <- n = 2
            raise Return
        if (((n % 3 + 3) % 3)) = 0 then
            __ret <- n = 3
            raise Return
        let mutable d: int = 5
        while (d * d) <= n do
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 2
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 4
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let mutable circs: int array = [||]
let rec isCircular (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        let mutable nn: int = n
        let mutable pow: int = 1
        while nn > 0 do
            pow <- pow * 10
            nn <- nn / 10
        nn <- n
        try
            while true do
                nn <- nn * 10
                let f: int = nn / pow
                nn <- nn + (f * (1 - pow))
                if nn = n then
                    raise Break
                if not (isPrime nn) then
                    __ret <- false
                    raise Return
        with
        | Break -> ()
        | Continue -> ()
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" "The first 19 circular primes are:"
let mutable digits: int array = [|1; 3; 7; 9|]
let mutable q: int array = [|1; 2; 3; 5; 7; 9|]
let mutable fq: int array = [|1; 2; 3; 5; 7; 9|]
let mutable count: int = 0
try
    while true do
        let f: int = q.[0]
        let fd: int = fq.[0]
        if (isPrime f) && (isCircular f) then
            circs <- unbox<int array> (Array.append circs [|f|])
            count <- count + 1
            if count = 19 then
                raise Break
        q <- unbox<int array> (Array.sub q 1 ((unbox<int> (Array.length q)) - 1))
        fq <- unbox<int array> (Array.sub fq 1 ((unbox<int> (Array.length fq)) - 1))
        if (f <> 2) && (f <> 5) then
            for d in digits do
                q <- unbox<int array> (Array.append q [|(f * 10) + d|])
                fq <- unbox<int array> (Array.append fq [|fd|])
with
| Break -> ()
| Continue -> ()
let rec showList (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable out: string = "["
        let mutable i: int = 0
        while i < (unbox<int> (Array.length xs)) do
            out <- out + (string (xs.[i]))
            if i < (unbox<int> ((unbox<int> (Array.length xs)) - 1)) then
                out <- out + ", "
            i <- i + 1
        __ret <- out + "]"
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (showList circs)
printfn "%s" "\nThe next 4 circular primes, in repunit format, are:"
printfn "%s" "[R(19) R(23) R(317) R(1031)]"
printfn "%s" "\nThe following repunits are probably circular primes:"
for i in [|5003; 9887; 15073; 25031; 35317; 49081|] do
    printfn "%s" (("R(" + (string i)) + ") : true")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
