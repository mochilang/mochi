// Generated 2025-08-12 07:47 +0700

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
type Fraction = {
    mutable _numerator: int
    mutable _denominator: int
}
let rec pow10 (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable result: int = 1
        let mutable i: int = 0
        while i < n do
            result <- int ((int64 result) * (int64 10))
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and gcd (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        if x < 0 then
            x <- -x
        if y < 0 then
            y <- -y
        while y <> 0 do
            let r: int = ((x % y + y) % y)
            x <- y
            y <- r
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and parse_decimal (s: string) =
    let mutable __ret : Fraction = Unchecked.defaultof<Fraction>
    let mutable s = s
    try
        if (String.length (s)) = 0 then
            failwith ("invalid number")
        let mutable idx: int = 0
        let mutable sign: int = 1
        let first: string = _substring s 0 1
        if first = "-" then
            sign <- -1
            idx <- 1
        else
            if first = "+" then
                idx <- 1
        let mutable int_part: string = ""
        try
            while idx < (String.length (s)) do
                try
                    let c: string = _substring s idx (idx + 1)
                    if (c >= "0") && (c <= "9") then
                        int_part <- int_part + c
                        idx <- idx + 1
                    else
                        raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable frac_part: string = ""
        if (idx < (String.length (s))) && ((_substring s idx (idx + 1)) = ".") then
            idx <- idx + 1
            try
                while idx < (String.length (s)) do
                    try
                        let c: string = _substring s idx (idx + 1)
                        if (c >= "0") && (c <= "9") then
                            frac_part <- frac_part + c
                            idx <- idx + 1
                        else
                            raise Break
                    with
                    | Continue -> ()
                    | Break -> raise Break
            with
            | Break -> ()
            | Continue -> ()
        let mutable exp: int = 0
        if (idx < (String.length (s))) && (((_substring s idx (idx + 1)) = "e") || ((_substring s idx (idx + 1)) = "E")) then
            idx <- idx + 1
            let mutable exp_sign: int = 1
            if (idx < (String.length (s))) && ((_substring s idx (idx + 1)) = "-") then
                exp_sign <- -1
                idx <- idx + 1
            else
                if (idx < (String.length (s))) && ((_substring s idx (idx + 1)) = "+") then
                    idx <- idx + 1
            let mutable exp_str: string = ""
            while idx < (String.length (s)) do
                let c: string = _substring s idx (idx + 1)
                if (c >= "0") && (c <= "9") then
                    exp_str <- exp_str + c
                    idx <- idx + 1
                else
                    failwith ("invalid number")
            if (String.length (exp_str)) = 0 then
                failwith ("invalid number")
            exp <- int ((int64 exp_sign) * (int64 (int (int (exp_str)))))
        if idx <> (String.length (s)) then
            failwith ("invalid number")
        if (String.length (int_part)) = 0 then
            int_part <- "0"
        let mutable num_str: string = int_part + frac_part
        let mutable _numerator: int = int (num_str)
        if sign = (0 - 1) then
            _numerator <- 0 - _numerator
        let mutable _denominator: int = pow10 (String.length (frac_part))
        if exp > 0 then
            _numerator <- int ((int64 _numerator) * (int64 (pow10 (exp))))
        else
            if exp < 0 then
                _denominator <- int ((int64 _denominator) * (int64 (pow10 (-exp))))
        __ret <- { _numerator = _numerator; _denominator = _denominator }
        raise Return
        __ret
    with
        | Return -> __ret
and reduce (fr: Fraction) =
    let mutable __ret : Fraction = Unchecked.defaultof<Fraction>
    let mutable fr = fr
    try
        let g: int = gcd (fr._numerator) (fr._denominator)
        __ret <- { _numerator = _floordiv (fr._numerator) g; _denominator = _floordiv (fr._denominator) g }
        raise Return
        __ret
    with
        | Return -> __ret
and decimal_to_fraction_str (s: string) =
    let mutable __ret : Fraction = Unchecked.defaultof<Fraction>
    let mutable s = s
    try
        __ret <- reduce (parse_decimal (s))
        raise Return
        __ret
    with
        | Return -> __ret
and decimal_to_fraction (x: float) =
    let mutable __ret : Fraction = Unchecked.defaultof<Fraction>
    let mutable x = x
    try
        __ret <- decimal_to_fraction_str (_str (x))
        raise Return
        __ret
    with
        | Return -> __ret
and assert_fraction (name: string) (fr: Fraction) (num: int) (den: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable name = name
    let mutable fr = fr
    let mutable num = num
    let mutable den = den
    try
        if ((fr._numerator) <> num) || ((fr._denominator) <> den) then
            failwith (name)
        __ret
    with
        | Return -> __ret
and test_decimal_to_fraction () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        assert_fraction ("case1") (decimal_to_fraction (2.0)) (2) (1)
        assert_fraction ("case2") (decimal_to_fraction (89.0)) (89) (1)
        assert_fraction ("case3") (decimal_to_fraction_str ("67")) (67) (1)
        assert_fraction ("case4") (decimal_to_fraction_str ("45.0")) (45) (1)
        assert_fraction ("case5") (decimal_to_fraction (1.5)) (3) (2)
        assert_fraction ("case6") (decimal_to_fraction_str ("6.25")) (25) (4)
        assert_fraction ("case7") (decimal_to_fraction (0.0)) (0) (1)
        assert_fraction ("case8") (decimal_to_fraction (-2.5)) (-5) (2)
        assert_fraction ("case9") (decimal_to_fraction (0.125)) (1) (8)
        assert_fraction ("case10") (decimal_to_fraction (1000000.25)) (4000001) (4)
        assert_fraction ("case11") (decimal_to_fraction (1.3333)) (13333) (10000)
        assert_fraction ("case12") (decimal_to_fraction_str ("1.23e2")) (123) (1)
        assert_fraction ("case13") (decimal_to_fraction_str ("0.500")) (1) (2)
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_decimal_to_fraction()
        let fr: Fraction = decimal_to_fraction (1.5)
        printfn "%s" (((_str (fr._numerator)) + "/") + (_str (fr._denominator)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
