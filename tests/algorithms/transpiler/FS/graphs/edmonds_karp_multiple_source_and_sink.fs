// Generated 2025-08-14 09:59 +0700

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
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
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
let rec push_relabel_max_flow (graph: int array array) (sources: int array) (sinks: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable graph = graph
    let mutable sources = sources
    let mutable sinks = sinks
    try
        if ((Seq.length (sources)) = 0) || ((Seq.length (sinks)) = 0) then
            __ret <- 0
            raise Return
        let mutable g: int array array = graph
        let mutable source_index: int = _idx sources (int 0)
        let mutable sink_index: int = _idx sinks (int 0)
        if ((Seq.length (sources)) > 1) || ((Seq.length (sinks)) > 1) then
            let mutable max_input_flow: int = 0
            let mutable i: int = 0
            while i < (Seq.length (sources)) do
                let mutable j: int = 0
                while j < (Seq.length (_idx g (int (_idx sources (int i))))) do
                    max_input_flow <- max_input_flow + (_idx (_idx g (int (_idx sources (int i)))) (int j))
                    j <- j + 1
                i <- i + 1
            let mutable size: int = (Seq.length (g)) + 1
            let mutable new_graph: int array array = Array.empty<int array>
            let mutable zero_row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < size do
                zero_row <- Array.append zero_row [|0|]
                j <- j + 1
            new_graph <- Array.append new_graph [|zero_row|]
            let mutable r: int = 0
            while r < (Seq.length (g)) do
                let mutable row: int array = unbox<int array> [|0|]
                let mutable c: int = 0
                while c < (Seq.length (_idx g (int r))) do
                    row <- Array.append row [|(_idx (_idx g (int r)) (int c))|]
                    c <- c + 1
                new_graph <- Array.append new_graph [|row|]
                r <- r + 1
            g <- new_graph
            i <- 0
            while i < (Seq.length (sources)) do
                g.[0].[((_idx sources (int i)) + 1)] <- max_input_flow
                i <- i + 1
            source_index <- 0
            size <- (Seq.length (g)) + 1
            new_graph <- Array.empty<int array>
            r <- 0
            while r < (Seq.length (g)) do
                let mutable row2: int array = _idx g (int r)
                row2 <- Array.append row2 [|0|]
                new_graph <- Array.append new_graph [|row2|]
                r <- r + 1
            let mutable last_row: int array = Array.empty<int>
            j <- 0
            while j < size do
                last_row <- Array.append last_row [|0|]
                j <- j + 1
            new_graph <- Array.append new_graph [|last_row|]
            g <- new_graph
            i <- 0
            while i < (Seq.length (sinks)) do
                g.[((_idx sinks (int i)) + 1)].[(size - 1)] <- max_input_flow
                i <- i + 1
            sink_index <- size - 1
        let n: int = Seq.length (g)
        let mutable preflow: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < n do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < n do
                row <- Array.append row [|0|]
                j <- j + 1
            preflow <- Array.append preflow [|row|]
            i <- i + 1
        let mutable heights: int array = Array.empty<int>
        i <- 0
        while i < n do
            heights <- Array.append heights [|0|]
            i <- i + 1
        let mutable excesses: int array = Array.empty<int>
        i <- 0
        while i < n do
            excesses <- Array.append excesses [|0|]
            i <- i + 1
        heights.[source_index] <- n
        i <- 0
        while i < n do
            let mutable bandwidth: int = _idx (_idx g (int source_index)) (int i)
            preflow.[source_index].[i] <- (_idx (_idx preflow (int source_index)) (int i)) + bandwidth
            preflow.[i].[source_index] <- (_idx (_idx preflow (int i)) (int source_index)) - bandwidth
            excesses.[i] <- (_idx excesses (int i)) + bandwidth
            i <- i + 1
        let mutable vertices_list: int array = Array.empty<int>
        i <- 0
        while i < n do
            if (i <> source_index) && (i <> sink_index) then
                vertices_list <- Array.append vertices_list [|i|]
            i <- i + 1
        let mutable idx: int = 0
        try
            while idx < (Seq.length (vertices_list)) do
                try
                    let mutable v: int = _idx vertices_list (int idx)
                    let mutable prev_height: int = _idx heights (int v)
                    try
                        while (_idx excesses (int v)) > 0 do
                            try
                                let mutable nb: int = 0
                                while nb < n do
                                    if (((_idx (_idx g (int v)) (int nb)) - (_idx (_idx preflow (int v)) (int nb))) > 0) && ((_idx heights (int v)) > (_idx heights (int nb))) then
                                        let mutable delta: int = _idx excesses (int v)
                                        let mutable capacity: int = (_idx (_idx g (int v)) (int nb)) - (_idx (_idx preflow (int v)) (int nb))
                                        if delta > capacity then
                                            delta <- capacity
                                        preflow.[v].[nb] <- (_idx (_idx preflow (int v)) (int nb)) + delta
                                        preflow.[nb].[v] <- (_idx (_idx preflow (int nb)) (int v)) - delta
                                        excesses.[v] <- (_idx excesses (int v)) - delta
                                        excesses.[nb] <- (_idx excesses (int nb)) + delta
                                    nb <- nb + 1
                                let mutable min_height: int = -1
                                nb <- 0
                                while nb < n do
                                    if ((_idx (_idx g (int v)) (int nb)) - (_idx (_idx preflow (int v)) (int nb))) > 0 then
                                        if (min_height = (-1)) || ((_idx heights (int nb)) < min_height) then
                                            min_height <- _idx heights (int nb)
                                    nb <- nb + 1
                                if min_height <> (-1) then
                                    heights.[v] <- min_height + 1
                                else
                                    raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if (_idx heights (int v)) > prev_height then
                        let mutable vertex: int = _idx vertices_list (int idx)
                        let mutable j: int = idx
                        while j > 0 do
                            vertices_list.[j] <- _idx vertices_list (int (j - 1))
                            j <- j - 1
                        vertices_list.[0] <- vertex
                        idx <- 0
                    else
                        idx <- idx + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable flow: int = 0
        i <- 0
        while i < n do
            flow <- flow + (_idx (_idx preflow (int source_index)) (int i))
            i <- i + 1
        if flow < 0 then
            flow <- -flow
        __ret <- flow
        raise Return
        __ret
    with
        | Return -> __ret
let graph: int array array = [|[|0; 7; 0; 0|]; [|0; 0; 6; 0|]; [|0; 0; 0; 8|]; [|9; 0; 0; 0|]|]
let sources: int array = unbox<int array> [|0|]
let sinks: int array = unbox<int array> [|3|]
let result: int = push_relabel_max_flow (graph) (sources) (sinks)
ignore (printfn "%s" ("maximum flow is " + (_str (result))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
