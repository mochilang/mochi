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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Network = {
    mutable _w1: float array array
    mutable _w2: float array array
    mutable _w3: float array array
}
let rec exp_approx (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable sum: float = 1.0
        let mutable term: float = 1.0
        let mutable i: int = 1
        while i < 10 do
            term <- float ((term * x) / (float (float (i))))
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and sigmoid (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- 1.0 / (1.0 + (exp_approx (-x)))
        raise Return
        __ret
    with
        | Return -> __ret
and sigmoid_derivative (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- x * (1.0 - x)
        raise Return
        __ret
    with
        | Return -> __ret
and new_network () =
    let mutable __ret : Network = Unchecked.defaultof<Network>
    try
        __ret <- { _w1 = [|[|0.1; 0.2; 0.3; 0.4|]; [|0.5; 0.6; 0.7; 0.8|]; [|0.9; 1.0; 1.1; 1.2|]|]; _w2 = [|[|0.1; 0.2; 0.3|]; [|0.4; 0.5; 0.6|]; [|0.7; 0.8; 0.9|]; [|1.0; 1.1; 1.2|]|]; _w3 = [|[|0.1|]; [|0.2|]; [|0.3|]|] }
        raise Return
        __ret
    with
        | Return -> __ret
and feedforward (net: Network) (input: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable net = net
    let mutable input = input
    try
        let mutable hidden1: float array = Array.empty<float>
        let mutable j: int = 0
        while j < 4 do
            let mutable sum1: float = 0.0
            let mutable i: int = 0
            while i < 3 do
                sum1 <- sum1 + ((_idx input (int i)) * (_idx (_idx (net._w1) (int i)) (int j)))
                i <- i + 1
            hidden1 <- Array.append hidden1 [|(sigmoid (sum1))|]
            j <- j + 1
        let mutable hidden2: float array = Array.empty<float>
        let mutable k: int = 0
        while k < 3 do
            let mutable sum2: float = 0.0
            let mutable j2: int = 0
            while j2 < 4 do
                sum2 <- sum2 + ((_idx hidden1 (int j2)) * (_idx (_idx (net._w2) (int j2)) (int k)))
                j2 <- j2 + 1
            hidden2 <- Array.append hidden2 [|(sigmoid (sum2))|]
            k <- k + 1
        let mutable sum3: float = 0.0
        let mutable k2: int = 0
        while k2 < 3 do
            sum3 <- sum3 + ((_idx hidden2 (int k2)) * (_idx (_idx (net._w3) (int k2)) (int 0)))
            k2 <- k2 + 1
        let out: float = sigmoid (sum3)
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and train (net: Network) (inputs: float array array) (outputs: float array) (iterations: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable net = net
    let mutable inputs = inputs
    let mutable outputs = outputs
    let mutable iterations = iterations
    try
        let mutable iter: int = 0
        while iter < iterations do
            let mutable s: int = 0
            while s < (Seq.length (inputs)) do
                let inp: float array = _idx inputs (int s)
                let target: float = _idx outputs (int s)
                let mutable hidden1: float array = Array.empty<float>
                let mutable j: int = 0
                while j < 4 do
                    let mutable sum1: float = 0.0
                    let mutable i: int = 0
                    while i < 3 do
                        sum1 <- sum1 + ((_idx inp (int i)) * (_idx (_idx (net._w1) (int i)) (int j)))
                        i <- i + 1
                    hidden1 <- Array.append hidden1 [|(sigmoid (sum1))|]
                    j <- j + 1
                let mutable hidden2: float array = Array.empty<float>
                let mutable k: int = 0
                while k < 3 do
                    let mutable sum2: float = 0.0
                    let mutable j2: int = 0
                    while j2 < 4 do
                        sum2 <- sum2 + ((_idx hidden1 (int j2)) * (_idx (_idx (net._w2) (int j2)) (int k)))
                        j2 <- j2 + 1
                    hidden2 <- Array.append hidden2 [|(sigmoid (sum2))|]
                    k <- k + 1
                let mutable sum3: float = 0.0
                let mutable k3: int = 0
                while k3 < 3 do
                    sum3 <- sum3 + ((_idx hidden2 (int k3)) * (_idx (_idx (net._w3) (int k3)) (int 0)))
                    k3 <- k3 + 1
                let output: float = sigmoid (sum3)
                let error: float = target - output
                let delta_output: float = error * (sigmoid_derivative (output))
                let mutable new_w3: float array array = Array.empty<float array>
                let mutable k4: int = 0
                while k4 < 3 do
                    let mutable w3row: float array = _idx (net._w3) (int k4)
                    w3row.[int 0] <- (_idx w3row (int 0)) + ((_idx hidden2 (int k4)) * delta_output)
                    new_w3 <- Array.append new_w3 [|w3row|]
                    k4 <- k4 + 1
                net._w3 <- new_w3
                let mutable delta_hidden2: float array = Array.empty<float>
                let mutable k5: int = 0
                while k5 < 3 do
                    let row: float array = _idx (net._w3) (int k5)
                    let dh2: float = ((_idx row (int 0)) * delta_output) * (sigmoid_derivative (_idx hidden2 (int k5)))
                    delta_hidden2 <- Array.append delta_hidden2 [|dh2|]
                    k5 <- k5 + 1
                let mutable new_w2: float array array = Array.empty<float array>
                j <- 0
                while j < 4 do
                    let mutable w2row: float array = _idx (net._w2) (int j)
                    let mutable k6: int = 0
                    while k6 < 3 do
                        w2row.[int k6] <- (_idx w2row (int k6)) + ((_idx hidden1 (int j)) * (_idx delta_hidden2 (int k6)))
                        k6 <- k6 + 1
                    new_w2 <- Array.append new_w2 [|w2row|]
                    j <- j + 1
                net._w2 <- new_w2
                let mutable delta_hidden1: float array = Array.empty<float>
                j <- 0
                while j < 4 do
                    let mutable sumdh: float = 0.0
                    let mutable k7: int = 0
                    while k7 < 3 do
                        let row2: float array = _idx (net._w2) (int j)
                        sumdh <- sumdh + ((_idx row2 (int k7)) * (_idx delta_hidden2 (int k7)))
                        k7 <- k7 + 1
                    delta_hidden1 <- Array.append delta_hidden1 [|(sumdh * (sigmoid_derivative (_idx hidden1 (int j))))|]
                    j <- j + 1
                let mutable new_w1: float array array = Array.empty<float array>
                let mutable i2: int = 0
                while i2 < 3 do
                    let mutable w1row: float array = _idx (net._w1) (int i2)
                    j <- 0
                    while j < 4 do
                        w1row.[int j] <- (_idx w1row (int j)) + ((_idx inp (int i2)) * (_idx delta_hidden1 (int j)))
                        j <- j + 1
                    new_w1 <- Array.append new_w1 [|w1row|]
                    i2 <- i2 + 1
                net._w1 <- new_w1
                s <- s + 1
            iter <- iter + 1
        __ret
    with
        | Return -> __ret
and predict (net: Network) (input: float array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable net = net
    let mutable input = input
    try
        let out: float = feedforward (net) (input)
        if out > 0.6 then
            __ret <- 1
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and example () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        let inputs: float array array = [|[|0.0; 0.0; 0.0|]; [|0.0; 0.0; 1.0|]; [|0.0; 1.0; 0.0|]; [|0.0; 1.0; 1.0|]; [|1.0; 0.0; 0.0|]; [|1.0; 0.0; 1.0|]; [|1.0; 1.0; 0.0|]; [|1.0; 1.0; 1.0|]|]
        let outputs: float array = unbox<float array> [|0.0; 1.0; 1.0; 0.0; 1.0; 0.0; 0.0; 1.0|]
        let mutable net: Network = new_network()
        train (net) (inputs) (outputs) (10)
        let result: int = predict (net) (unbox<float array> [|1.0; 1.0; 1.0|])
        printfn "%s" (_str (result))
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        example()
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
