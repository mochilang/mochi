// Generated 2025-08-06 22:46 +0700

exception Break
exception Continue

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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let WIDTH: int = 10
let HEIGHT: int = 10
let PREY_INITIAL_COUNT: int = 20
let PREY_REPRODUCTION_TIME: int = 5
let PREDATOR_INITIAL_COUNT: int = 5
let PREDATOR_REPRODUCTION_TIME: int = 20
let PREDATOR_INITIAL_ENERGY: int = 15
let PREDATOR_FOOD_VALUE: int = 5
let TYPE_PREY: int = 0
let TYPE_PREDATOR: int = 1
let mutable seed: int = 123456789
let rec rand () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        seed <- int ((((int64 ((seed * 1103515245) + 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- seed
        raise Return
        __ret
    with
        | Return -> __ret
and rand_range (max: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable max = max
    try
        __ret <- (((rand()) % max + max) % max)
        raise Return
        __ret
    with
        | Return -> __ret
and shuffle (list_int: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable list_int = list_int
    try
        let mutable i: int = (Seq.length (list_int)) - 1
        while i > 0 do
            let mutable j: int = rand_range (i + 1)
            let tmp: int = _idx list_int (i)
            list_int <- _arrset list_int i (_idx list_int (j))
            list_int <- _arrset list_int j (tmp)
            i <- i - 1
        __ret <- list_int
        raise Return
        __ret
    with
        | Return -> __ret
and create_board () =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    try
        let mutable board: int array array = [||]
        let mutable r: int = 0
        while r < HEIGHT do
            let mutable row: int array = [||]
            let mutable c: int = 0
            while c < WIDTH do
                row <- Array.append row [|0|]
                c <- c + 1
            board <- Array.append board [|row|]
            r <- r + 1
        __ret <- board
        raise Return
        __ret
    with
        | Return -> __ret
and create_prey (r: int) (c: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable r = r
    let mutable c = c
    try
        __ret <- unbox<int array> [|TYPE_PREY; r; c; PREY_REPRODUCTION_TIME; 0; 1|]
        raise Return
        __ret
    with
        | Return -> __ret
and create_predator (r: int) (c: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable r = r
    let mutable c = c
    try
        __ret <- unbox<int array> [|TYPE_PREDATOR; r; c; PREDATOR_REPRODUCTION_TIME; PREDATOR_INITIAL_ENERGY; 1|]
        raise Return
        __ret
    with
        | Return -> __ret
let mutable board: int array array = create_board()
let mutable entities: int array array = [||]
let rec empty_cell (r: int) (c: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable r = r
    let mutable c = c
    try
        __ret <- (_idx (_idx board (r)) (c)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
and add_entity (typ: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable typ = typ
    try
        while true do
            let mutable r: int = rand_range (HEIGHT)
            let mutable c: int = rand_range (WIDTH)
            if empty_cell (r) (c) then
                if typ = TYPE_PREY then
                    board.[r].[c] <- 1
                    entities <- Array.append entities [|create_prey (r) (c)|]
                else
                    board.[r].[c] <- 2
                    entities <- Array.append entities [|create_predator (r) (c)|]
                __ret <- ()
                raise Return
        __ret
    with
        | Return -> __ret
and setup () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable i: int = 0
        while i < PREY_INITIAL_COUNT do
            add_entity (TYPE_PREY)
            i <- i + 1
        i <- 0
        while i < PREDATOR_INITIAL_COUNT do
            add_entity (TYPE_PREDATOR)
            i <- i + 1
        __ret
    with
        | Return -> __ret
let dr: int array = [|-1; 0; 1; 0|]
let dc: int array = [|0; 1; 0; -1|]
let rec inside (r: int) (c: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable r = r
    let mutable c = c
    try
        __ret <- (((r >= 0) && (r < HEIGHT)) && (c >= 0)) && (c < WIDTH)
        raise Return
        __ret
    with
        | Return -> __ret
and find_prey (r: int) (c: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable r = r
    let mutable c = c
    try
        let mutable i: int = 0
        while i < (Seq.length (entities)) do
            let e: int array = _idx entities (i)
            if ((((_idx e (5)) = 1) && ((_idx e (0)) = TYPE_PREY)) && ((_idx e (1)) = r)) && ((_idx e (2)) = c) then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and step_world () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable i: int = 0
        try
            while i < (Seq.length (entities)) do
                try
                    let mutable e: int array = _idx entities (i)
                    if (_idx e (5)) = 0 then
                        i <- i + 1
                        raise Continue
                    let typ: int = _idx e (0)
                    let mutable row: int = _idx e (1)
                    let col: int = _idx e (2)
                    let repro: int = _idx e (3)
                    let energy: int = _idx e (4)
                    let mutable dirs: int array = [|0; 1; 2; 3|]
                    dirs <- shuffle (dirs)
                    let mutable moved: bool = false
                    let old_r: int = row
                    let old_c: int = col
                    if typ = TYPE_PREDATOR then
                        let mutable j: int = 0
                        let mutable ate: bool = false
                        try
                            while j < 4 do
                                try
                                    let d: int = _idx dirs (j)
                                    let nr: int = row + (_idx dr (d))
                                    let nc: int = col + (_idx dc (d))
                                    if (inside (nr) (nc)) && ((_idx (_idx board (nr)) (nc)) = 1) then
                                        let prey_index: int = find_prey (nr) (nc)
                                        if prey_index >= 0 then
                                            entities.[prey_index].[5] <- 0
                                        board.[nr].[nc] <- 2
                                        board.[row].[col] <- 0
                                        e <- _arrset e 1 (nr)
                                        e <- _arrset e 2 (nc)
                                        e <- _arrset e 4 ((energy + PREDATOR_FOOD_VALUE) - 1)
                                        moved <- true
                                        ate <- true
                                        raise Break
                                    j <- j + 1
                                with
                                | Continue -> ()
                                | Break -> raise Break
                        with
                        | Break -> ()
                        | Continue -> ()
                        if not ate then
                            j <- 0
                            try
                                while j < 4 do
                                    try
                                        let d: int = _idx dirs (j)
                                        let nr: int = row + (_idx dr (d))
                                        let nc: int = col + (_idx dc (d))
                                        if (inside (nr) (nc)) && ((_idx (_idx board (nr)) (nc)) = 0) then
                                            board.[nr].[nc] <- 2
                                            board.[row].[col] <- 0
                                            e <- _arrset e 1 (nr)
                                            e <- _arrset e 2 (nc)
                                            moved <- true
                                            raise Break
                                        j <- j + 1
                                    with
                                    | Continue -> ()
                                    | Break -> raise Break
                            with
                            | Break -> ()
                            | Continue -> ()
                            e <- _arrset e 4 (energy - 1)
                        if (_idx e (4)) <= 0 then
                            e <- _arrset e 5 (0)
                            board.[_idx e (1)].[_idx e (2)] <- 0
                    else
                        let mutable j: int = 0
                        try
                            while j < 4 do
                                try
                                    let d: int = _idx dirs (j)
                                    let nr: int = row + (_idx dr (d))
                                    let nc: int = col + (_idx dc (d))
                                    if (inside (nr) (nc)) && ((_idx (_idx board (nr)) (nc)) = 0) then
                                        board.[nr].[nc] <- 1
                                        board.[row].[col] <- 0
                                        e <- _arrset e 1 (nr)
                                        e <- _arrset e 2 (nc)
                                        moved <- true
                                        raise Break
                                    j <- j + 1
                                with
                                | Continue -> ()
                                | Break -> raise Break
                        with
                        | Break -> ()
                        | Continue -> ()
                    if (_idx e (5)) = 1 then
                        if moved && (repro <= 0) then
                            if typ = TYPE_PREY then
                                board.[old_r].[old_c] <- 1
                                entities <- Array.append entities [|create_prey (old_r) (old_c)|]
                                e <- _arrset e 3 (PREY_REPRODUCTION_TIME)
                            else
                                board.[old_r].[old_c] <- 2
                                entities <- Array.append entities [|create_predator (old_r) (old_c)|]
                                e <- _arrset e 3 (PREDATOR_REPRODUCTION_TIME)
                        else
                            e <- _arrset e 3 (repro - 1)
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable alive: int array array = [||]
        let mutable k: int = 0
        while k < (Seq.length (entities)) do
            let e2: int array = _idx entities (k)
            if (_idx e2 (5)) = 1 then
                alive <- Array.append alive [|e2|]
            k <- k + 1
        entities <- alive
        __ret
    with
        | Return -> __ret
and count_entities (typ: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable typ = typ
    try
        let mutable cnt: int = 0
        let mutable i: int = 0
        while i < (Seq.length (entities)) do
            if ((_idx (_idx entities (i)) (0)) = typ) && ((_idx (_idx entities (i)) (5)) = 1) then
                cnt <- cnt + 1
            i <- i + 1
        __ret <- cnt
        raise Return
        __ret
    with
        | Return -> __ret
setup()
let mutable t: int = 0
while t < 10 do
    step_world()
    t <- t + 1
printfn "%s" ("Prey: " + (_str (count_entities (TYPE_PREY))))
printfn "%s" ("Predators: " + (_str (count_entities (TYPE_PREDATOR))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
