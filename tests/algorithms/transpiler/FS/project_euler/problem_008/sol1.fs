// Generated 2025-08-23 14:49 +0700

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

let rec _str v =
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let N: string = (((((((((((((((((("73167176531330624919225119674426574742355349194934" + "96983520312774506326239578318016984801869478851843") + "85861560789112949495459501737958331952853208805511") + "12540698747158523863050715693290963295227443043557") + "66896648950445244523161731856403098711121722383113") + "62229893423380308135336276614282806444486645238749") + "30358907296290491560440772390713810515859307960866") + "70172427121883998797908792274921901699720888093776") + "65727333001053367881220235421809751254540594752243") + "52584907711670556013604839586446706324415722155397") + "53697817977846174064955149290862569321978468622482") + "83972241375657056057490261407972968652414535100474") + "82166370484403199890008895243450658541227588666881") + "16427171479924442928230863465674813919123162824586") + "17866458359124566529476545682848912883142607690042") + "24219022671055626321111109370544217506941658960408") + "07198403850962455444362981230987879927244284909188") + "84580156166097919133875499200524063689912560717606") + "05886116467109405077541002256983155200055935729725") + "71636269561882670428252483600823257530420752963450"
let rec solution (n: string) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        let mutable largest_product: int64 = int64 0
        let mutable i: int64 = int64 0
        while i <= (int64 ((String.length (n)) - 13)) do
            let mutable product: int64 = int64 1
            let mutable j: int64 = int64 0
            while j < (int64 13) do
                product <- product * (int (_substring n (int64 (i + j)) (int64 ((i + j) + (int64 1)))))
                j <- j + (int64 1)
            if product > largest_product then
                largest_product <- product
            i <- i + (int64 1)
        __ret <- largest_product
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" ("solution() = " + (_str (solution (N)))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
