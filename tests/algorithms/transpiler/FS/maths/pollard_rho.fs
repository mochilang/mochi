// Generated 2025-08-08 18:09 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type PollardResult = {
    mutable _factor: int
    mutable _ok: bool
}
let rec gcd (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = if a < 0 then (-a) else a
        let mutable y: int = if b < 0 then (-b) else b
        while y <> 0 do
            let t: int = ((x % y + y) % y)
            x <- y
            y <- t
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and rand_fn (value: int) (step: int) (modulus: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable value = value
    let mutable step = step
    let mutable modulus = modulus
    try
        __ret <- int ((((((int64 value) * (int64 value)) + (int64 step)) % (int64 modulus) + (int64 modulus)) % (int64 modulus)))
        raise Return
        __ret
    with
        | Return -> __ret
and pollard_rho (num: int) (_seed: int) (step: int) (attempts: int) =
    let mutable __ret : PollardResult = Unchecked.defaultof<PollardResult>
    let mutable num = num
    let mutable _seed = _seed
    let mutable step = step
    let mutable attempts = attempts
    try
        if num < 2 then
            failwith ("The input value cannot be less than 2")
        if (num > 2) && ((((num % 2 + 2) % 2)) = 0) then
            __ret <- { _factor = 2; _ok = true }
            raise Return
        let mutable s: int = _seed
        let mutable st: int = step
        let mutable i: int = 0
        try
            while i < attempts do
                try
                    let mutable tortoise: int = s
                    let mutable hare: int = s
                    try
                        while true do
                            try
                                tortoise <- rand_fn (tortoise) (st) (num)
                                hare <- rand_fn (hare) (st) (num)
                                hare <- rand_fn (hare) (st) (num)
                                let divisor: int = gcd (hare - tortoise) (num)
                                if divisor = 1 then
                                    raise Continue
                                else
                                    if divisor = num then
                                        raise Break
                                    else
                                        __ret <- { _factor = divisor; _ok = true }
                                        raise Return
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    s <- hare
                    st <- st + 1
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- { _factor = 0; _ok = false }
        raise Return
        __ret
    with
        | Return -> __ret
and test_pollard_rho () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let r1: PollardResult = pollard_rho (8051) (2) (1) (5)
        if (not (r1._ok)) || (((r1._factor) <> 83) && ((r1._factor) <> 97)) then
            failwith ("test1 failed")
        let r2: PollardResult = pollard_rho (10403) (2) (1) (5)
        if (not (r2._ok)) || (((r2._factor) <> 101) && ((r2._factor) <> 103)) then
            failwith ("test2 failed")
        let r3: PollardResult = pollard_rho (100) (2) (1) (3)
        if (not (r3._ok)) || ((r3._factor) <> 2) then
            failwith ("test3 failed")
        let r4: PollardResult = pollard_rho (17) (2) (1) (3)
        if r4._ok then
            failwith ("test4 failed")
        let r5: PollardResult = pollard_rho (int (((int64 17) * (int64 17)) * (int64 17))) (2) (1) (3)
        if (not (r5._ok)) || ((r5._factor) <> 17) then
            failwith ("test5 failed")
        let r6: PollardResult = pollard_rho (int (((int64 17) * (int64 17)) * (int64 17))) (2) (1) (1)
        if r6._ok then
            failwith ("test6 failed")
        let r7: PollardResult = pollard_rho (int (((int64 3) * (int64 5)) * (int64 7))) (2) (1) (3)
        if (not (r7._ok)) || ((r7._factor) <> 21) then
            failwith ("test7 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_pollard_rho()
        let a: PollardResult = pollard_rho (100) (2) (1) (3)
        if a._ok then
            printfn "%s" (_str (a._factor))
        else
            printfn "%s" ("None")
        let b: PollardResult = pollard_rho (17) (2) (1) (3)
        if b._ok then
            printfn "%s" (_str (b._factor))
        else
            printfn "%s" ("None")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
