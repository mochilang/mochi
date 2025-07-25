// Generated 2025-07-25 22:14 +0700

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
let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" "Diagram after trimming whitespace and removal of blank lines:\n"
        printfn "%s" "+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+"
        printfn "%s" "|                      ID                       |"
        printfn "%s" "+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+"
        printfn "%s" "|QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |"
        printfn "%s" "+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+"
        printfn "%s" "|                    QDCOUNT                    |"
        printfn "%s" "+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+"
        printfn "%s" "|                    ANCOUNT                    |"
        printfn "%s" "+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+"
        printfn "%s" "|                    NSCOUNT                    |"
        printfn "%s" "+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+"
        printfn "%s" "|                    ARCOUNT                    |"
        printfn "%s" "+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+"
        printfn "%s" "\nDecoded:\n"
        printfn "%s" "Name     Bits  Start  End"
        printfn "%s" "=======  ====  =====  ==="
        printfn "%s" "ID        16      0    15"
        printfn "%s" "QR         1     16    16"
        printfn "%s" "Opcode     4     17    20"
        printfn "%s" "AA         1     21    21"
        printfn "%s" "TC         1     22    22"
        printfn "%s" "RD         1     23    23"
        printfn "%s" "RA         1     24    24"
        printfn "%s" "Z          3     25    27"
        printfn "%s" "RCODE      4     28    31"
        printfn "%s" "QDCOUNT   16     32    47"
        printfn "%s" "ANCOUNT   16     48    63"
        printfn "%s" "NSCOUNT   16     64    79"
        printfn "%s" "ARCOUNT   16     80    95"
        printfn "%s" "\nTest string in hex:"
        printfn "%s" "78477bbf5496e12e1bf169a4"
        printfn "%s" "\nTest string in binary:"
        printfn "%s" "011110000100011101111011101111110101010010010110111000010010111000011011111100010110100110100100"
        printfn "%s" "\nUnpacked:\n"
        printfn "%s" "Name     Size  Bit pattern"
        printfn "%s" "=======  ====  ================"
        printfn "%s" "ID        16   0111100001000111"
        printfn "%s" "QR         1   0"
        printfn "%s" "Opcode     4   1111"
        printfn "%s" "AA         1   0"
        printfn "%s" "TC         1   1"
        printfn "%s" "RD         1   1"
        printfn "%s" "RA         1   1"
        printfn "%s" "Z          3   011"
        printfn "%s" "RCODE      4   1111"
        printfn "%s" "QDCOUNT   16   0101010010010110"
        printfn "%s" "ANCOUNT   16   1110000100101110"
        printfn "%s" "NSCOUNT   16   0001101111110001"
        printfn "%s" "ARCOUNT   16   0110100110100100"
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
