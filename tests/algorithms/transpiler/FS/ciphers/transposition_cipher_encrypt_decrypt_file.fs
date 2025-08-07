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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec encrypt_message (key: int) (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    let mutable message = message
    try
        let mutable result: string = ""
        let mutable col: int = 0
        while col < key do
            let mutable pointer: int = col
            while pointer < (String.length (message)) do
                result <- result + (string (message.[pointer]))
                pointer <- pointer + key
            col <- col + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec decrypt_message (key: int) (message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    let mutable message = message
    try
        let msg_len: int = String.length (message)
        let mutable num_cols: int = msg_len / key
        if (((msg_len % key + key) % key)) <> 0 then
            num_cols <- num_cols + 1
        let num_rows: int = key
        let num_shaded_boxes: int = (num_cols * num_rows) - msg_len
        let mutable plain: string array = [||]
        let mutable i: int = 0
        while i < num_cols do
            plain <- Array.append plain [|""|]
            i <- i + 1
        let mutable col: int = 0
        let mutable row: int = 0
        let mutable idx: int = 0
        while idx < msg_len do
            let ch: string = string (message.[idx])
            plain.[col] <- (_idx plain (col)) + ch
            col <- col + 1
            if (col = num_cols) || ((col = (num_cols - 1)) && (row >= (num_rows - num_shaded_boxes))) then
                col <- 0
                row <- row + 1
            idx <- idx + 1
        let mutable result: string = ""
        i <- 0
        while i < num_cols do
            result <- result + (_idx plain (i))
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let key: int = 6
let message: string = "Harshil Darji"
let encrypted: string = encrypt_message (key) (message)
printfn "%s" (encrypted)
let decrypted: string = decrypt_message (key) (encrypted)
printfn "%s" (decrypted)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
