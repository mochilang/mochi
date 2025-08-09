// Generated 2025-08-09 10:14 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type DataSet = {
    mutable images: int array array
    mutable labels: int array array
    mutable _num_examples: int
    mutable _index_in_epoch: int
    mutable _epochs_completed: int
}
type Datasets = {
    mutable _train: DataSet
    mutable _validation: DataSet
    mutable _test_ds: DataSet
}
type BatchResult = {
    mutable _dataset: DataSet
    mutable images: int array array
    mutable labels: int array array
}
let rec dense_to_one_hot (labels: int array) (num_classes: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable labels = labels
    let mutable num_classes = num_classes
    try
        let mutable result: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < (Seq.length (labels)) do
            let mutable row: int array = Array.empty<int>
            let mutable j: int = 0
            while j < num_classes do
                if j = (_idx labels (int i)) then
                    row <- Array.append row [|1|]
                else
                    row <- Array.append row [|0|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and new_dataset (images: int array array) (labels: int array array) =
    let mutable __ret : DataSet = Unchecked.defaultof<DataSet>
    let mutable images = images
    let mutable labels = labels
    try
        __ret <- { images = images; labels = labels; _num_examples = Seq.length (images); _index_in_epoch = 0; _epochs_completed = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
and next_batch (ds: DataSet) (batch_size: int) =
    let mutable __ret : BatchResult = Unchecked.defaultof<BatchResult>
    let mutable ds = ds
    let mutable batch_size = batch_size
    try
        let start: int = ds._index_in_epoch
        if (start + batch_size) > (ds._num_examples) then
            let rest: int = (ds._num_examples) - start
            let images_rest: int array array = Array.sub ds.images start ((ds._num_examples) - start)
            let labels_rest: int array array = Array.sub ds.labels start ((ds._num_examples) - start)
            let new_index: int = batch_size - rest
            let images_new: int array array = Array.sub ds.images 0 (new_index - 0)
            let labels_new: int array array = Array.sub ds.labels 0 (new_index - 0)
            let batch_images: int array array = Array.append (images_rest) (images_new)
            let batch_labels: int array array = Array.append (labels_rest) (labels_new)
            let new_ds: DataSet = { images = ds.images; labels = ds.labels; _num_examples = ds._num_examples; _index_in_epoch = new_index; _epochs_completed = (ds._epochs_completed) + 1 }
            __ret <- { _dataset = new_ds; images = batch_images; labels = batch_labels }
            raise Return
        else
            let ``end``: int = start + batch_size
            let batch_images: int array array = Array.sub ds.images start (``end`` - start)
            let batch_labels: int array array = Array.sub ds.labels start (``end`` - start)
            let new_ds: DataSet = { images = ds.images; labels = ds.labels; _num_examples = ds._num_examples; _index_in_epoch = ``end``; _epochs_completed = ds._epochs_completed }
            __ret <- { _dataset = new_ds; images = batch_images; labels = batch_labels }
            raise Return
        __ret
    with
        | Return -> __ret
and read_data_sets (train_images: int array array) (train_labels_raw: int array) (test_images: int array array) (test_labels_raw: int array) (validation_size: int) (num_classes: int) =
    let mutable __ret : Datasets = Unchecked.defaultof<Datasets>
    let mutable train_images = train_images
    let mutable train_labels_raw = train_labels_raw
    let mutable test_images = test_images
    let mutable test_labels_raw = test_labels_raw
    let mutable validation_size = validation_size
    let mutable num_classes = num_classes
    try
        let train_labels: int array array = dense_to_one_hot (train_labels_raw) (num_classes)
        let test_labels: int array array = dense_to_one_hot (test_labels_raw) (num_classes)
        let validation_images: int array array = Array.sub train_images 0 (validation_size - 0)
        let validation_labels: int array array = Array.sub train_labels 0 (validation_size - 0)
        let train_images_rest: int array array = Array.sub train_images validation_size ((Seq.length (train_images)) - validation_size)
        let train_labels_rest: int array array = Array.sub train_labels validation_size ((Seq.length (train_labels)) - validation_size)
        let _train: DataSet = new_dataset (train_images_rest) (train_labels_rest)
        let _validation: DataSet = new_dataset (validation_images) (validation_labels)
        let testset: DataSet = new_dataset (test_images) (test_labels)
        __ret <- { _train = _train; _validation = _validation; _test_ds = testset }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let train_images: int array array = [|[|0; 1|]; [|1; 2|]; [|2; 3|]; [|3; 4|]; [|4; 5|]|]
        let train_labels_raw: int array = unbox<int array> [|0; 1; 2; 3; 4|]
        let test_images: int array array = [|[|5; 6|]; [|6; 7|]|]
        let test_labels_raw: int array = unbox<int array> [|5; 6|]
        let data: Datasets = read_data_sets (train_images) (train_labels_raw) (test_images) (test_labels_raw) (2) (10)
        let mutable ds: DataSet = data._train
        let mutable res: BatchResult = next_batch (ds) (2)
        ds <- res._dataset
        printfn "%s" (_str (res.images))
        printfn "%s" (_str (res.labels))
        res <- next_batch (ds) (2)
        ds <- res._dataset
        printfn "%s" (_str (res.images))
        printfn "%s" (_str (res.labels))
        res <- next_batch (ds) (2)
        ds <- res._dataset
        printfn "%s" (_str (res.images))
        printfn "%s" (_str (res.labels))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
