// Generated 2025-08-04 17:04 +0700

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
open System.Diagnostics

open System

module subprocess =
    let rec getoutput (cmd: string) =
        let mutable __ret : string = Unchecked.defaultof<string>
        let mutable cmd = cmd
        try
            let psi = System.Diagnostics.ProcessStartInfo()
            psi.FileName <- "/bin/sh"
            psi.Arguments <- "-c " + (unbox<string> cmd)
            psi.RedirectStandardOutput <- true
            psi.UseShellExecute <- false
            let p = System.Diagnostics.Process.Start (psi)
            let output = p.StandardOutput.ReadToEnd()
            p.WaitForExit()
            __ret <- output.TrimEnd()
            __ret
        with
            | Return -> __ret

let out: string = subprocess.getoutput("ls -l")
printfn "%s" (out)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
