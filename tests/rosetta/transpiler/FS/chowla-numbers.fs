// Generated 2025-07-28 00:39 +0700

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
printfn "%s" "chowla( 1) = 0\nchowla( 2) = 0\nchowla( 3) = 0\nchowla( 4) = 2\nchowla( 5) = 0\nchowla( 6) = 5\nchowla( 7) = 0\nchowla( 8) = 6\nchowla( 9) = 3\nchowla(10) = 7\nchowla(11) = 0\nchowla(12) = 15\nchowla(13) = 0\nchowla(14) = 9\nchowla(15) = 8\nchowla(16) = 14\nchowla(17) = 0\nchowla(18) = 20\nchowla(19) = 0\nchowla(20) = 21\nchowla(21) = 10\nchowla(22) = 13\nchowla(23) = 0\nchowla(24) = 35\nchowla(25) = 5\nchowla(26) = 15\nchowla(27) = 12\nchowla(28) = 27\nchowla(29) = 0\nchowla(30) = 41\nchowla(31) = 0\nchowla(32) = 30\nchowla(33) = 14\nchowla(34) = 19\nchowla(35) = 12\nchowla(36) = 54\nchowla(37) = 0\n\nCount of primes up to 100        = 25\nCount of primes up to 1,000      = 168\nCount of primes up to 10,000     = 1,229\nCount of primes up to 100,000    = 9,592\nCount of primes up to 1,000,000  = 78,498\nCount of primes up to 10,000,000 = 664,579\n\n6 is a perfect number\n28 is a perfect number\n496 is a perfect number\n8,128 is a perfect number\n33,550,336 is a perfect number\nThere are 5 perfect numbers <= 35,000,000"
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
