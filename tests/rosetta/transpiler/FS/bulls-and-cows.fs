// Generated 2025-07-27 15:57 +0700

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

open System

let rec indexOf (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length s) do
            if (_substring s i (i + 1)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and shuffle (xs: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable xs = xs
    try
        let mutable arr: string array = xs
        let mutable i: int = (unbox<int> (Array.length arr)) - 1
        while i > 0 do
            let j = (((_now()) % (i + 1) + (i + 1)) % (i + 1))
            let tmp: string = arr.[i]
            arr.[i] <- arr.[j]
            arr.[j] <- tmp
            i <- i - 1
        __ret <- unbox<string array> arr
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" "Cows and Bulls"
        printfn "%s" "Guess four digit number of unique digits in the range 1 to 9."
        printfn "%s" "A correct digit but not in the correct place is a cow."
        printfn "%s" "A correct digit in the correct place is a bull."
        let mutable digits: string array = [|"1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"|]
        digits <- shuffle digits
        let mutable pat = (((digits.[0]) + (digits.[1])) + (digits.[2])) + (digits.[3])
        let valid: string = "123456789"
        try
            while true do
                printfn "%s" "Guess: "
                let guess: string = System.Console.ReadLine()
                if (String.length guess) <> 4 then
                    printfn "%s" "Please guess a four digit number."
                    raise Continue
                let mutable cows: int = 0
                let mutable bulls: int = 0
                let mutable seen: string = ""
                let mutable i: int = 0
                let mutable malformed: bool = false
                try
                    while i < 4 do
                        let cg: string = _substring guess i (i + 1)
                        if (unbox<int> (indexOf seen cg)) <> (-1) then
                            printfn "%s" ("Repeated digit: " + cg)
                            malformed <- true
                            raise Break
                        seen <- seen + cg
                        let pos: int = indexOf (unbox<string> pat) cg
                        if pos = (-1) then
                            if (unbox<int> (indexOf valid cg)) = (-1) then
                                printfn "%s" ("Invalid digit: " + cg)
                                malformed <- true
                                raise Break
                        else
                            if pos = i then
                                bulls <- bulls + 1
                            else
                                cows <- cows + 1
                        i <- i + 1
                with
                | Break -> ()
                | Continue -> ()
                if malformed then
                    raise Continue
                printfn "%s" ((("Cows: " + (string cows)) + ", bulls: ") + (string bulls))
                if bulls = 4 then
                    printfn "%s" "You got it."
                    raise Break
        with
        | Break -> ()
        | Continue -> ()
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
