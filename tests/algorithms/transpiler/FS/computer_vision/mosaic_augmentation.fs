// Generated 2025-08-07 10:31 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type MosaicResult = {
    img: int array array
    annos: float array array
    path: string
}
let rec update_image_and_anno (all_img_list: string array) (all_annos: float array array array) (idxs: int array) (output_size: int array) (scale_range: float array) (filter_scale: float) =
    let mutable __ret : MosaicResult = Unchecked.defaultof<MosaicResult>
    let mutable all_img_list = all_img_list
    let mutable all_annos = all_annos
    let mutable idxs = idxs
    let mutable output_size = output_size
    let mutable scale_range = scale_range
    let mutable filter_scale = filter_scale
    try
        let height: int = _idx output_size (0)
        let width: int = _idx output_size (1)
        let mutable output_img: int array array = Array.empty<int array>
        let mutable r: int = 0
        while r < height do
            let mutable row: int array = Array.empty<int>
            let mutable c: int = 0
            while c < width do
                row <- Array.append row [|0|]
                c <- c + 1
            output_img <- Array.append output_img [|row|]
            r <- r + 1
        let scale_x: float = ((_idx scale_range (0)) + (_idx scale_range (1))) / 2.0
        let scale_y: float = ((_idx scale_range (0)) + (_idx scale_range (1))) / 2.0
        let divid_point_x: int = int (scale_x * (float width))
        let divid_point_y: int = int (scale_y * (float height))
        let mutable new_anno: float array array = Array.empty<float array>
        let mutable path_list: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (Seq.length (idxs)) do
            let index: int = _idx idxs (i)
            let path: string = _idx all_img_list (index)
            path_list <- Array.append path_list [|path|]
            let img_annos: float array array = _idx all_annos (index)
            if i = 0 then
                let mutable y0: int = 0
                while y0 < divid_point_y do
                    let mutable x0: int = 0
                    while x0 < divid_point_x do
                        output_img.[y0].[x0] <- i + 1
                        x0 <- x0 + 1
                    y0 <- y0 + 1
                let mutable j0: int = 0
                while j0 < (Seq.length (img_annos)) do
                    let bbox: float array = _idx img_annos (j0)
                    let xmin: float = (_idx bbox (1)) * scale_x
                    let ymin: float = (_idx bbox (2)) * scale_y
                    let xmax: float = (_idx bbox (3)) * scale_x
                    let ymax: float = (_idx bbox (4)) * scale_y
                    new_anno <- Array.append new_anno [|[|_idx bbox (0); xmin; ymin; xmax; ymax|]|]
                    j0 <- j0 + 1
            else
                if i = 1 then
                    let mutable y1: int = 0
                    while y1 < divid_point_y do
                        let mutable x1: int = divid_point_x
                        while x1 < width do
                            output_img.[y1].[x1] <- i + 1
                            x1 <- x1 + 1
                        y1 <- y1 + 1
                    let mutable j1: int = 0
                    while j1 < (Seq.length (img_annos)) do
                        let bbox1: float array = _idx img_annos (j1)
                        let xmin1: float = scale_x + ((_idx bbox1 (1)) * (1.0 - scale_x))
                        let ymin1: float = (_idx bbox1 (2)) * scale_y
                        let xmax1: float = scale_x + ((_idx bbox1 (3)) * (1.0 - scale_x))
                        let ymax1: float = (_idx bbox1 (4)) * scale_y
                        new_anno <- Array.append new_anno [|[|_idx bbox1 (0); xmin1; ymin1; xmax1; ymax1|]|]
                        j1 <- j1 + 1
                else
                    if i = 2 then
                        let mutable y2: int = divid_point_y
                        while y2 < height do
                            let mutable x2: int = 0
                            while x2 < divid_point_x do
                                output_img.[y2].[x2] <- i + 1
                                x2 <- x2 + 1
                            y2 <- y2 + 1
                        let mutable j2: int = 0
                        while j2 < (Seq.length (img_annos)) do
                            let bbox2: float array = _idx img_annos (j2)
                            let xmin2: float = (_idx bbox2 (1)) * scale_x
                            let ymin2: float = scale_y + ((_idx bbox2 (2)) * (1.0 - scale_y))
                            let xmax2: float = (_idx bbox2 (3)) * scale_x
                            let ymax2: float = scale_y + ((_idx bbox2 (4)) * (1.0 - scale_y))
                            new_anno <- Array.append new_anno [|[|_idx bbox2 (0); xmin2; ymin2; xmax2; ymax2|]|]
                            j2 <- j2 + 1
                    else
                        let mutable y3: int = divid_point_y
                        while y3 < height do
                            let mutable x3: int = divid_point_x
                            while x3 < width do
                                output_img.[y3].[x3] <- i + 1
                                x3 <- x3 + 1
                            y3 <- y3 + 1
                        let mutable j3: int = 0
                        while j3 < (Seq.length (img_annos)) do
                            let bbox3: float array = _idx img_annos (j3)
                            let xmin3: float = scale_x + ((_idx bbox3 (1)) * (1.0 - scale_x))
                            let ymin3: float = scale_y + ((_idx bbox3 (2)) * (1.0 - scale_y))
                            let xmax3: float = scale_x + ((_idx bbox3 (3)) * (1.0 - scale_x))
                            let ymax3: float = scale_y + ((_idx bbox3 (4)) * (1.0 - scale_y))
                            new_anno <- Array.append new_anno [|[|_idx bbox3 (0); xmin3; ymin3; xmax3; ymax3|]|]
                            j3 <- j3 + 1
            i <- i + 1
        if filter_scale > 0.0 then
            let mutable filtered: float array array = Array.empty<float array>
            let mutable k: int = 0
            while k < (Seq.length (new_anno)) do
                let anno: float array = _idx new_anno (k)
                let w: float = (_idx anno (3)) - (_idx anno (1))
                let h: float = (_idx anno (4)) - (_idx anno (2))
                if (filter_scale < w) && (filter_scale < h) then
                    filtered <- Array.append filtered [|anno|]
                k <- k + 1
            new_anno <- filtered
        __ret <- { img = output_img; annos = new_anno; path = _idx path_list (0) }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let all_img_list: string array = [|"img0.jpg"; "img1.jpg"; "img2.jpg"; "img3.jpg"|]
        let all_annos: float array array array = [|[|[|0.0; 0.1; 0.1; 0.4; 0.4|]|]; [|[|1.0; 0.2; 0.3; 0.5; 0.7|]|]; [|[|2.0; 0.6; 0.2; 0.9; 0.5|]|]; [|[|3.0; 0.5; 0.5; 0.8; 0.8|]|]|]
        let idxs: int array = [|0; 1; 2; 3|]
        let output_size: int array = [|100; 100|]
        let scale_range: float array = [|0.4; 0.6|]
        let filter_scale: float = 0.05
        let res: MosaicResult = update_image_and_anno (all_img_list) (all_annos) (idxs) (output_size) (scale_range) (filter_scale)
        let new_annos: float array array = res.annos
        let path: string = res.path
        printfn "%s" ("Base image: " + path)
        printfn "%s" ("Mosaic annotation count: " + (_str (Seq.length (new_annos))))
        let mutable i: int = 0
        while i < (Seq.length (new_annos)) do
            let a: float array = _idx new_annos (i)
            printfn "%s" (((((((((_str (_idx a (0))) + " ") + (_str (_idx a (1)))) + " ") + (_str (_idx a (2)))) + " ") + (_str (_idx a (3)))) + " ") + (_str (_idx a (4))))
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
