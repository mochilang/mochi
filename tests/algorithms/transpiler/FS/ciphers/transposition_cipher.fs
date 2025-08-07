// Generated 2025-08-07 10:31 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
open System

let rec join_strings (xs: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            res <- res + (_idx xs (i))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and encrypt_message (key: int) (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    let mutable message = message
    try
        let mutable result: string = ""
        let mutable col: int = 0
        while col < key do
            let mutable pointer: int = col
            while pointer < (String.length (message)) do
                result <- result + (_substring message pointer (pointer + 1))
                pointer <- pointer + key
            col <- col + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and decrypt_message (key: int) (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    let mutable message = message
    try
        let num_cols: int = (((String.length (message)) + key) - 1) / key
        let num_rows: int = key
        let num_shaded_boxes: int = (num_cols * num_rows) - (String.length (message))
        let mutable plain_text: string array = [||]
        let mutable i: int = 0
        while i < num_cols do
            plain_text <- Array.append plain_text [|""|]
            i <- i + 1
        let mutable col: int = 0
        let mutable row: int = 0
        let mutable index: int = 0
        while index < (String.length (message)) do
            plain_text.[col] <- (_idx plain_text (col)) + (_substring message index (index + 1))
            col <- col + 1
            if (col = num_cols) || ((col = (num_cols - 1)) && (row >= (num_rows - num_shaded_boxes))) then
                col <- 0
                row <- row + 1
            index <- index + 1
        __ret <- join_strings (plain_text)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" ("Enter message: ")
        let message: string = System.Console.ReadLine()
        let max_key: int = (String.length (message)) - 1
        printfn "%s" (("Enter key [2-" + (_str (max_key))) + "]: ")
        let key: int = int (System.Console.ReadLine())
        printfn "%s" ("Encryption/Decryption [e/d]: ")
        let mode: string = System.Console.ReadLine()
        let mutable text: string = ""
        let first: string = _substring mode 0 1
        if (first = "e") || (first = "E") then
            text <- encrypt_message (key) (message)
        else
            if (first = "d") || (first = "D") then
                text <- decrypt_message (key) (message)
        printfn "%s" (("Output:\n" + text) + "|")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
