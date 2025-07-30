// Generated 2025-07-30 21:41 +0700

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
printfn "%s" "<table>"
printfn "%s" "    <tr><td>Character</td><td>Speech</td></tr>"
printfn "%s" "    <tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>"
printfn "%s" "    <tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He&#39;s not the messiah; he&#39;s a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>"
printfn "%s" "    <tr><td>The multitude</td><td>Who are you?</td></tr>"
printfn "%s" "    <tr><td>Brians mother</td><td>I&#39;m his mother; that&#39;s who!</td></tr>"
printfn "%s" "    <tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>"
printfn "%s" "</table>"
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
