// Generated 2025-08-11 15:32 +0700

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

let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let MAX_LOCAL_PART_OCTETS: int = 64
let MAX_DOMAIN_OCTETS: int = 255
let ASCII_LETTERS: string = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
let DIGITS: string = "0123456789"
let LOCAL_EXTRA: string = ".(!#$%&'*+-/=?^_`{|}~)"
let DOMAIN_EXTRA: string = ".-"
let rec count_char (s: string) (target: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable target = target
    try
        let mutable cnt: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (_substring s i (i + 1)) = target then
                cnt <- cnt + 1
            i <- i + 1
        __ret <- cnt
        raise Return
        __ret
    with
        | Return -> __ret
let rec char_in (c: string) (allowed: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable c = c
    let mutable allowed = allowed
    try
        let mutable i: int = 0
        while i < (String.length (allowed)) do
            if (_substring allowed i (i + 1)) = c then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec starts_with_char (s: string) (c: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable c = c
    try
        __ret <- ((String.length (s)) > 0) && ((_substring s 0 1) = c)
        raise Return
        __ret
    with
        | Return -> __ret
let rec ends_with_char (s: string) (c: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable c = c
    try
        __ret <- ((String.length (s)) > 0) && ((_substring s ((String.length (s)) - 1) (String.length (s))) = c)
        raise Return
        __ret
    with
        | Return -> __ret
let rec contains_double_dot (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        if (String.length (s)) < 2 then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < ((String.length (s)) - 1) do
            if (_substring s i (i + 2)) = ".." then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_valid_email_address (email: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable email = email
    try
        if (count_char (email) ("@")) <> 1 then
            __ret <- false
            raise Return
        let mutable at_idx: int = 0
        let mutable i: int = 0
        try
            while i < (String.length (email)) do
                try
                    if (_substring email i (i + 1)) = "@" then
                        at_idx <- i
                        raise Break
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let local_part: string = _substring email 0 at_idx
        let domain: string = _substring email (at_idx + 1) (String.length (email))
        if ((String.length (local_part)) > MAX_LOCAL_PART_OCTETS) || ((String.length (domain)) > MAX_DOMAIN_OCTETS) then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < (String.length (local_part)) do
            let ch: string = _substring local_part i (i + 1)
            if not (char_in (ch) ((ASCII_LETTERS + DIGITS) + LOCAL_EXTRA)) then
                __ret <- false
                raise Return
            i <- i + 1
        if ((starts_with_char (local_part) (".")) || (ends_with_char (local_part) ("."))) || (contains_double_dot (local_part)) then
            __ret <- false
            raise Return
        i <- 0
        while i < (String.length (domain)) do
            let ch: string = _substring domain i (i + 1)
            if not (char_in (ch) ((ASCII_LETTERS + DIGITS) + DOMAIN_EXTRA)) then
                __ret <- false
                raise Return
            i <- i + 1
        if (starts_with_char (domain) ("-")) || (ends_with_char (domain) (".")) then
            __ret <- false
            raise Return
        if ((starts_with_char (domain) (".")) || (ends_with_char (domain) ("."))) || (contains_double_dot (domain)) then
            __ret <- false
            raise Return
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let email_tests: string array = unbox<string array> [|"simple@example.com"; "very.common@example.com"; "disposable.style.email.with+symbol@example.com"; "other-email-with-hyphen@and.subdomains.example.com"; "fully-qualified-domain@example.com"; "user.name+tag+sorting@example.com"; "x@example.com"; "example-indeed@strange-example.com"; "test/test@test.com"; "123456789012345678901234567890123456789012345678901234567890123@example.com"; "admin@mailserver1"; "example@s.example"; "Abc.example.com"; "A@b@c@example.com"; "abc@example..com"; "a(c)d,e:f;g<h>i[j\\k]l@example.com"; "12345678901234567890123456789012345678901234567890123456789012345@example.com"; "i.like.underscores@but_its_not_allowed_in_this_part"; ""|]
let mutable idx: int = 0
while idx < (Seq.length (email_tests)) do
    let email: string = _idx email_tests (int idx)
    printfn "%s" (_str (is_valid_email_address (email)))
    idx <- idx + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
