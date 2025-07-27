// Generated 2025-07-27 23:36 +0700

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
type Info = {
    animal: string
    yinYang: string
    element: string
    stemBranch: string
    cycle: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let animal: string array = [|"Rat"; "Ox"; "Tiger"; "Rabbit"; "Dragon"; "Snake"; "Horse"; "Goat"; "Monkey"; "Rooster"; "Dog"; "Pig"|]
let yinYang: string array = [|"Yang"; "Yin"|]
let element: string array = [|"Wood"; "Fire"; "Earth"; "Metal"; "Water"|]
let stemChArr: string array = [|"甲"; "乙"; "丙"; "丁"; "戊"; "己"; "庚"; "辛"; "壬"; "癸"|]
let branchChArr: string array = [|"子"; "丑"; "寅"; "卯"; "辰"; "巳"; "午"; "未"; "申"; "酉"; "戌"; "亥"|]
let rec cz (yr: int) (animal: string array) (yinYang: string array) (element: string array) (sc: string array) (bc: string array) =
    let mutable __ret : Info = Unchecked.defaultof<Info>
    let mutable yr = yr
    let mutable animal = animal
    let mutable yinYang = yinYang
    let mutable element = element
    let mutable sc = sc
    let mutable bc = bc
    try
        let mutable y: int = yr - 4
        let stem: int = ((y % 10 + 10) % 10)
        let branch: int = ((y % 12 + 12) % 12)
        let sb = (sc.[stem]) + (bc.[branch])
        __ret <- { animal = unbox<string> (animal.[branch]); yinYang = unbox<string> (yinYang.[((stem % 2 + 2) % 2)]); element = unbox<string> (element.[int (stem / 2)]); stemBranch = sb; cycle = (((y % 60 + 60) % 60)) + 1 }
        raise Return
        __ret
    with
        | Return -> __ret
for yr in [|1935; 1938; 1968; 1972; 1976|] do
    let r: Info = cz (int yr) animal yinYang element stemChArr branchChArr
    printfn "%s" (((((((((((string yr) + ": ") + (r.element)) + " ") + (r.animal)) + ", ") + (r.yinYang)) + ", Cycle year ") + (string (r.cycle))) + " ") + (r.stemBranch))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
