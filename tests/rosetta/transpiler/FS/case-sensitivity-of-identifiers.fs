// Generated 2025-07-27 23:45 +0700

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
let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable pkg_dog: string = "Salt"
        let mutable Dog: string = "Pepper"
        let mutable pkg_DOG: string = "Mustard"
        let rec packageSees (d1: string) (d2: string) (d3: string) =
            let mutable __ret : Map<string, bool> = Unchecked.defaultof<Map<string, bool>>
            let mutable d1 = d1
            let mutable d2 = d2
            let mutable d3 = d3
            try
                printfn "%s" ((((("Package sees: " + d1) + " ") + d2) + " ") + d3)
                __ret <- Map.ofList [("pkg_dog", true); ("Dog", true); ("pkg_DOG", true)]
                raise Return
                __ret
            with
                | Return -> __ret
        let mutable d: Map<string, bool> = packageSees pkg_dog Dog pkg_DOG
        printfn "%s" (("There are " + (string (Seq.length d))) + " dogs.\n")
        let mutable dog: string = "Benjamin"
        d <- packageSees pkg_dog Dog pkg_DOG
        printfn "%s" ((((("Main sees:   " + dog) + " ") + Dog) + " ") + pkg_DOG)
        d <- Map.add "dog" true d
        d <- Map.add "Dog" true d
        d <- Map.add "pkg_DOG" true d
        printfn "%s" (("There are " + (string (Seq.length d))) + " dogs.\n")
        Dog <- "Samba"
        d <- packageSees pkg_dog Dog pkg_DOG
        printfn "%s" ((((("Main sees:   " + dog) + " ") + Dog) + " ") + pkg_DOG)
        d <- Map.add "dog" true d
        d <- Map.add "Dog" true d
        d <- Map.add "pkg_DOG" true d
        printfn "%s" (("There are " + (string (Seq.length d))) + " dogs.\n")
        let mutable DOG: string = "Bernie"
        d <- packageSees pkg_dog Dog pkg_DOG
        printfn "%s" ((((("Main sees:   " + dog) + " ") + Dog) + " ") + DOG)
        d <- Map.add "dog" true d
        d <- Map.add "Dog" true d
        d <- Map.add "pkg_DOG" true d
        d <- Map.add "DOG" true d
        printfn "%s" (("There are " + (string (Seq.length d))) + " dogs.")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
