// Generated 2025-08-17 12:28 +0700

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
    | :? float as f -> sprintf "%.15g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec abs_int (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- if x < 0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and gcd_iter (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = abs_int (a)
        let mutable y: int = abs_int (b)
        while y <> 0 do
            let t: int = y
            y <- ((x % y + y) % y)
            x <- t
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and is_prime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n <= 1 then
            __ret <- false
            raise Return
        let mutable d: int = 2
        while (d * d) <= n do
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and sieve_er (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable nums: int array = Array.empty<int>
        let mutable i: int = 2
        while i <= n do
            nums <- Array.append nums [|i|]
            i <- i + 1
        let mutable idx: int = 0
        while idx < (Seq.length (nums)) do
            let mutable j: int = idx + 1
            while j < (Seq.length (nums)) do
                if (_idx nums (int idx)) <> 0 then
                    if ((((_idx nums (int j)) % (_idx nums (int idx)) + (_idx nums (int idx))) % (_idx nums (int idx)))) = 0 then
                        nums.[j] <- 0
                j <- j + 1
            idx <- idx + 1
        let mutable res: int array = Array.empty<int>
        let mutable k: int = 0
        while k < (Seq.length (nums)) do
            let v: int = _idx nums (int k)
            if v <> 0 then
                res <- Array.append res [|v|]
            k <- k + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and get_prime_numbers (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable ans: int array = Array.empty<int>
        let mutable num: int = 2
        while num <= n do
            if is_prime (num) then
                ans <- Array.append ans [|num|]
            num <- num + 1
        __ret <- ans
        raise Return
        __ret
    with
        | Return -> __ret
and prime_factorization (number: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable number = number
    try
        if number = 0 then
            __ret <- unbox<int array> [|0|]
            raise Return
        if number = 1 then
            __ret <- unbox<int array> [|1|]
            raise Return
        let mutable ans: int array = Array.empty<int>
        if is_prime (number) then
            ans <- Array.append ans [|number|]
            __ret <- ans
            raise Return
        let mutable quotient: int = number
        let mutable factor: int = 2
        while quotient <> 1 do
            if (is_prime (factor)) && ((((quotient % factor + factor) % factor)) = 0) then
                ans <- Array.append ans [|factor|]
                quotient <- _floordiv (int quotient) (int factor)
            else
                factor <- factor + 1
        __ret <- ans
        raise Return
        __ret
    with
        | Return -> __ret
and greatest_prime_factor (number: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number = number
    try
        let factors: int array = prime_factorization (number)
        let mutable m: int = _idx factors (int 0)
        let mutable i: int = 1
        while i < (Seq.length (factors)) do
            if (_idx factors (int i)) > m then
                m <- _idx factors (int i)
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
and smallest_prime_factor (number: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number = number
    try
        let factors: int array = prime_factorization (number)
        let mutable m: int = _idx factors (int 0)
        let mutable i: int = 1
        while i < (Seq.length (factors)) do
            if (_idx factors (int i)) < m then
                m <- _idx factors (int i)
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
and kg_v (number1: int) (number2: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number1 = number1
    let mutable number2 = number2
    try
        if (number1 < 1) || (number2 < 1) then
            ignore (failwith ("numbers must be positive"))
        let g: int = gcd_iter (number1) (number2)
        __ret <- (_floordiv (int number1) (int g)) * number2
        raise Return
        __ret
    with
        | Return -> __ret
and is_even (number: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable number = number
    try
        __ret <- (((number % 2 + 2) % 2)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
and is_odd (number: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable number = number
    try
        __ret <- (((number % 2 + 2) % 2)) <> 0
        raise Return
        __ret
    with
        | Return -> __ret
and goldbach (number: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable number = number
    try
        if (not (is_even (number))) || (number <= 2) then
            ignore (failwith ("number must be even and > 2"))
        let primes: int array = get_prime_numbers (number)
        let mutable i: int = 0
        while i < (Seq.length (primes)) do
            let mutable j: int = i + 1
            while j < (Seq.length (primes)) do
                if ((_idx primes (int i)) + (_idx primes (int j))) = number then
                    __ret <- unbox<int array> [|_idx primes (int i); _idx primes (int j)|]
                    raise Return
                j <- j + 1
            i <- i + 1
        __ret <- Array.empty<int>
        raise Return
        __ret
    with
        | Return -> __ret
and get_prime (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 0 then
            ignore (failwith ("n must be non-negative"))
        let mutable index: int = 0
        let mutable ans: int = 2
        while index < n do
            index <- index + 1
            ans <- ans + 1
            while not (is_prime (ans)) do
                ans <- ans + 1
        __ret <- ans
        raise Return
        __ret
    with
        | Return -> __ret
and get_primes_between (p1: int) (p2: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable p1 = p1
    let mutable p2 = p2
    try
        let bad1: bool = not (is_prime (p1))
        let bad2: bool = not (is_prime (p2))
        if (bad1 || bad2) || (p1 >= p2) then
            ignore (failwith ("arguments must be prime and p1 < p2"))
        let mutable num: int = p1 + 1
        try
            while num < p2 do
                try
                    if is_prime (num) then
                        raise Break
                    num <- num + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable ans: int array = Array.empty<int>
        try
            while num < p2 do
                try
                    ans <- Array.append ans [|num|]
                    num <- num + 1
                    try
                        while num < p2 do
                            try
                                if is_prime (num) then
                                    raise Break
                                num <- num + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- ans
        raise Return
        __ret
    with
        | Return -> __ret
and get_divisors (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        if n < 1 then
            ignore (failwith ("n must be >= 1"))
        let mutable ans: int array = Array.empty<int>
        let mutable d: int = 1
        while d <= n do
            if (((n % d + d) % d)) = 0 then
                ans <- Array.append ans [|d|]
            d <- d + 1
        __ret <- ans
        raise Return
        __ret
    with
        | Return -> __ret
and is_perfect_number (number: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable number = number
    try
        if number <= 1 then
            ignore (failwith ("number must be > 1"))
        let divisors: int array = get_divisors (number)
        let mutable sum: int = 0
        let mutable i: int = 0
        while i < ((Seq.length (divisors)) - 1) do
            sum <- sum + (_idx divisors (int i))
            i <- i + 1
        __ret <- sum = number
        raise Return
        __ret
    with
        | Return -> __ret
and simplify_fraction (numerator: int) (denominator: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable numerator = numerator
    let mutable denominator = denominator
    try
        if denominator = 0 then
            ignore (failwith ("denominator cannot be zero"))
        let g: int = gcd_iter (abs_int (numerator)) (abs_int (denominator))
        __ret <- unbox<int array> [|_floordiv (int numerator) (int g); _floordiv (int denominator) (int g)|]
        raise Return
        __ret
    with
        | Return -> __ret
and factorial (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 0 then
            ignore (failwith ("n must be >= 0"))
        let mutable ans: int = 1
        let mutable i: int = 1
        while i <= n do
            ans <- ans * i
            i <- i + 1
        __ret <- ans
        raise Return
        __ret
    with
        | Return -> __ret
and fib (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 0 then
            ignore (failwith ("n must be >= 0"))
        if n <= 1 then
            __ret <- 1
            raise Return
        let mutable tmp: int = 0
        let mutable fib1: int = 1
        let mutable ans: int = 1
        let mutable i: int = 0
        while i < (n - 1) do
            tmp <- ans
            ans <- ans + fib1
            fib1 <- tmp
            i <- i + 1
        __ret <- ans
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (is_prime (97))))
ignore (printfn "%s" (_str (sieve_er (20))))
ignore (printfn "%s" (_str (get_prime_numbers (20))))
ignore (printfn "%s" (_str (prime_factorization (287))))
ignore (printfn "%s" (_str (greatest_prime_factor (287))))
ignore (printfn "%s" (_str (smallest_prime_factor (287))))
ignore (printfn "%s" (_str (kg_v (8) (10))))
ignore (printfn "%s" (_str (goldbach (28))))
ignore (printfn "%s" (_str (get_prime (8))))
ignore (printfn "%s" (_str (get_primes_between (3) (23))))
ignore (printfn "%s" (_str (get_divisors (28))))
ignore (printfn "%s" (_str (is_perfect_number (28))))
ignore (printfn "%s" (_str (simplify_fraction (10) (20))))
ignore (printfn "%s" (_str (factorial (5))))
ignore (printfn "%s" (_str (fib (10))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
