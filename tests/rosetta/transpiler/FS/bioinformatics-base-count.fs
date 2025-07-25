// Generated 2025-07-26 04:38 +0700

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
let rec padLeft (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable res: string = ""
        let mutable n: int = w - (String.length s)
        while n > 0 do
            res <- res + " "
            n <- n - 1
        __ret <- res + s
        raise Return
        __ret
    with
        | Return -> __ret
let dna: string = ((((((((("" + "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATG") + "CTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTG") + "AGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGAT") + "GGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT") + "CGATTCTGCTTATAACACTATGTTCTTATGAAATGGATGTTCTGAGTTGG") + "TCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA") + "TTTAATTTTTCTATATAGCGATCTGTATTTAAGCAATTCATTTAGGTTAT") + "CGCCGCGATGCTCGGTTCGGACCGCCAAGCATCTGGCTCCACTGCTAGTG") + "TCCTAAATTTGAATGGCAAACACAAATAAGATTTAGCAATTCGTGTAGAC") + "GACCGGGGACTTGCATGATGGGAGCAGCTTTGTTAAACTACGAACGTAAT"
printfn "%s" "SEQUENCE:"
let le: int = String.length dna
let mutable i: int = 0
while i < le do
    let mutable k: int = i + 50
    if k > le then
        k <- le
    printfn "%s" (((unbox<string> (padLeft (string i) 5)) + ": ") + (dna.Substring(i, k - i)))
    i <- i + 50
let mutable a: int = 0
let mutable c: int = 0
let mutable g: int = 0
let mutable t: int = 0
let mutable idx: int = 0
while idx < le do
    let ch: string = dna.Substring(idx, (idx + 1) - idx)
    if ch = "A" then
        a <- a + 1
    else
        if ch = "C" then
            c <- c + 1
        else
            if ch = "G" then
                g <- g + 1
            else
                if ch = "T" then
                    t <- t + 1
    idx <- idx + 1
printfn "%s" ""
printfn "%s" "BASE COUNT:"
printfn "%s" ("    A: " + (unbox<string> (padLeft (string a) 3)))
printfn "%s" ("    C: " + (unbox<string> (padLeft (string c) 3)))
printfn "%s" ("    G: " + (unbox<string> (padLeft (string g) 3)))
printfn "%s" ("    T: " + (unbox<string> (padLeft (string t) 3)))
printfn "%s" "    ------"
printfn "%s" ("    Σ: " + (string le))
printfn "%s" "    ======"
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
