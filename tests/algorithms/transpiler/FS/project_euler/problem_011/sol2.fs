// Generated 2025-08-23 15:31 +0700

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
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
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
let grid: int64 array array = [|[|int64 (8L); int64 (2L); int64 (22L); int64 (97L); int64 (38L); int64 (15L); int64 (0L); int64 (40L); int64 (0L); int64 (75L); int64 (4L); int64 (5L); int64 (7L); int64 (78L); int64 (52L); int64 (12L); int64 (50L); int64 (77L); int64 (91L); int64 (8L)|]; [|int64 (49L); int64 (49L); int64 (99L); int64 (40L); int64 (17L); int64 (81L); int64 (18L); int64 (57L); int64 (60L); int64 (87L); int64 (17L); int64 (40L); int64 (98L); int64 (43L); int64 (69L); int64 (48L); int64 (4L); int64 (56L); int64 (62L); int64 (0L)|]; [|int64 (81L); int64 (49L); int64 (31L); int64 (73L); int64 (55L); int64 (79L); int64 (14L); int64 (29L); int64 (93L); int64 (71L); int64 (40L); int64 (67L); int64 (53L); int64 (88L); int64 (30L); int64 (3L); int64 (49L); int64 (13L); int64 (36L); int64 (65L)|]; [|int64 (52L); int64 (70L); int64 (95L); int64 (23L); int64 (4L); int64 (60L); int64 (11L); int64 (42L); int64 (69L); int64 (24L); int64 (68L); int64 (56L); int64 (1L); int64 (32L); int64 (56L); int64 (71L); int64 (37L); int64 (2L); int64 (36L); int64 (91L)|]; [|int64 (22L); int64 (31L); int64 (16L); int64 (71L); int64 (51L); int64 (67L); int64 (63L); int64 (89L); int64 (41L); int64 (92L); int64 (36L); int64 (54L); int64 (22L); int64 (40L); int64 (40L); int64 (28L); int64 (66L); int64 (33L); int64 (13L); int64 (80L)|]; [|int64 (24L); int64 (47L); int64 (32L); int64 (60L); int64 (99L); int64 (3L); int64 (45L); int64 (2L); int64 (44L); int64 (75L); int64 (33L); int64 (53L); int64 (78L); int64 (36L); int64 (84L); int64 (20L); int64 (35L); int64 (17L); int64 (12L); int64 (50L)|]; [|int64 (32L); int64 (98L); int64 (81L); int64 (28L); int64 (64L); int64 (23L); int64 (67L); int64 (10L); int64 (26L); int64 (38L); int64 (40L); int64 (67L); int64 (59L); int64 (54L); int64 (70L); int64 (66L); int64 (18L); int64 (38L); int64 (64L); int64 (70L)|]; [|int64 (67L); int64 (26L); int64 (20L); int64 (68L); int64 (2L); int64 (62L); int64 (12L); int64 (20L); int64 (95L); int64 (63L); int64 (94L); int64 (39L); int64 (63L); int64 (8L); int64 (40L); int64 (91L); int64 (66L); int64 (49L); int64 (94L); int64 (21L)|]; [|int64 (24L); int64 (55L); int64 (58L); int64 (5L); int64 (66L); int64 (73L); int64 (99L); int64 (26L); int64 (97L); int64 (17L); int64 (78L); int64 (78L); int64 (96L); int64 (83L); int64 (14L); int64 (88L); int64 (34L); int64 (89L); int64 (63L); int64 (72L)|]; [|int64 (21L); int64 (36L); int64 (23L); int64 (9L); int64 (75L); int64 (0L); int64 (76L); int64 (44L); int64 (20L); int64 (45L); int64 (35L); int64 (14L); int64 (0L); int64 (61L); int64 (33L); int64 (97L); int64 (34L); int64 (31L); int64 (33L); int64 (95L)|]; [|int64 (78L); int64 (17L); int64 (53L); int64 (28L); int64 (22L); int64 (75L); int64 (31L); int64 (67L); int64 (15L); int64 (94L); int64 (3L); int64 (80L); int64 (4L); int64 (62L); int64 (16L); int64 (14L); int64 (9L); int64 (53L); int64 (56L); int64 (92L)|]; [|int64 (16L); int64 (39L); int64 (5L); int64 (42L); int64 (96L); int64 (35L); int64 (31L); int64 (47L); int64 (55L); int64 (58L); int64 (88L); int64 (24L); int64 (0L); int64 (17L); int64 (54L); int64 (24L); int64 (36L); int64 (29L); int64 (85L); int64 (57L)|]; [|int64 (86L); int64 (56L); int64 (0L); int64 (48L); int64 (35L); int64 (71L); int64 (89L); int64 (7L); int64 (5L); int64 (44L); int64 (44L); int64 (37L); int64 (44L); int64 (60L); int64 (21L); int64 (58L); int64 (51L); int64 (54L); int64 (17L); int64 (58L)|]; [|int64 (19L); int64 (80L); int64 (81L); int64 (68L); int64 (5L); int64 (94L); int64 (47L); int64 (69L); int64 (28L); int64 (73L); int64 (92L); int64 (13L); int64 (86L); int64 (52L); int64 (17L); int64 (77L); int64 (4L); int64 (89L); int64 (55L); int64 (40L)|]; [|int64 (4L); int64 (52L); int64 (8L); int64 (83L); int64 (97L); int64 (35L); int64 (99L); int64 (16L); int64 (7L); int64 (97L); int64 (57L); int64 (32L); int64 (16L); int64 (26L); int64 (26L); int64 (79L); int64 (33L); int64 (27L); int64 (98L); int64 (66L)|]; [|int64 (88L); int64 (36L); int64 (68L); int64 (87L); int64 (57L); int64 (62L); int64 (20L); int64 (72L); int64 (3L); int64 (46L); int64 (33L); int64 (67L); int64 (46L); int64 (55L); int64 (12L); int64 (32L); int64 (63L); int64 (93L); int64 (53L); int64 (69L)|]; [|int64 (4L); int64 (42L); int64 (16L); int64 (73L); int64 (38L); int64 (25L); int64 (39L); int64 (11L); int64 (24L); int64 (94L); int64 (72L); int64 (18L); int64 (8L); int64 (46L); int64 (29L); int64 (32L); int64 (40L); int64 (62L); int64 (76L); int64 (36L)|]; [|int64 (20L); int64 (69L); int64 (36L); int64 (41L); int64 (72L); int64 (30L); int64 (23L); int64 (88L); int64 (34L); int64 (62L); int64 (99L); int64 (69L); int64 (82L); int64 (67L); int64 (59L); int64 (85L); int64 (74L); int64 (4L); int64 (36L); int64 (16L)|]; [|int64 (20L); int64 (73L); int64 (35L); int64 (29L); int64 (78L); int64 (31L); int64 (90L); int64 (1L); int64 (74L); int64 (31L); int64 (49L); int64 (71L); int64 (48L); int64 (86L); int64 (81L); int64 (16L); int64 (23L); int64 (57L); int64 (5L); int64 (54L)|]; [|int64 (1L); int64 (70L); int64 (54L); int64 (71L); int64 (83L); int64 (51L); int64 (54L); int64 (69L); int64 (16L); int64 (92L); int64 (33L); int64 (48L); int64 (61L); int64 (43L); int64 (52L); int64 (1L); int64 (89L); int64 (19L); int64 (67L); int64 (48L)|]|]
let rec max_product_four (grid: int64 array array) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable grid = grid
    try
        let mutable maximum: int64 = 0L
        let mutable i: int64 = 0L
        while i < 20L do
            let mutable j: int64 = 0L
            while j < 17L do
                let temp: int64 = (((_idx (_idx grid (int i)) (int j)) * (_idx (_idx grid (int i)) (int (j + 1L)))) * (_idx (_idx grid (int i)) (int (j + 2L)))) * (_idx (_idx grid (int i)) (int (j + 3L)))
                if temp > maximum then
                    maximum <- temp
                j <- j + 1L
            i <- i + 1L
        i <- 0L
        while i < 17L do
            let mutable j: int64 = 0L
            while j < 20L do
                let temp: int64 = (((_idx (_idx grid (int i)) (int j)) * (_idx (_idx grid (int (i + 1L))) (int j))) * (_idx (_idx grid (int (i + 2L))) (int j))) * (_idx (_idx grid (int (i + 3L))) (int j))
                if temp > maximum then
                    maximum <- temp
                j <- j + 1L
            i <- i + 1L
        i <- 0L
        while i < 17L do
            let mutable j: int64 = 0L
            while j < 17L do
                let temp: int64 = (((_idx (_idx grid (int i)) (int j)) * (_idx (_idx grid (int (i + 1L))) (int (j + 1L)))) * (_idx (_idx grid (int (i + 2L))) (int (j + 2L)))) * (_idx (_idx grid (int (i + 3L))) (int (j + 3L)))
                if temp > maximum then
                    maximum <- temp
                j <- j + 1L
            i <- i + 1L
        i <- 0L
        while i < 17L do
            let mutable j: int64 = 3L
            while j < 20L do
                let temp: int64 = (((_idx (_idx grid (int i)) (int j)) * (_idx (_idx grid (int (i + 1L))) (int (j - 1L)))) * (_idx (_idx grid (int (i + 2L))) (int (j - 2L)))) * (_idx (_idx grid (int (i + 3L))) (int (j - 3L)))
                if temp > maximum then
                    maximum <- temp
                j <- j + 1L
            i <- i + 1L
        __ret <- maximum
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (max_product_four (grid))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
