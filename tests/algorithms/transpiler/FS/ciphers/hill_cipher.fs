// Generated 2025-08-07 10:31 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let KEY_STRING: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
let rec mod36 (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable r: int = ((n % 36 + 36) % 36)
        if r < 0 then
            r <- r + 36
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
let rec gcd (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        while y <> 0 do
            let t: int = y
            y <- ((x % y + y) % y)
            x <- t
        if x < 0 then
            x <- -x
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
let rec replace_letters (letter: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable letter = letter
    try
        let mutable i: int = 0
        while i < (String.length (KEY_STRING)) do
            if (string (KEY_STRING.[i])) = letter then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec replace_digits (num: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable num = num
    try
        let idx: int = mod36 (num)
        __ret <- string (KEY_STRING.[idx])
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_upper (c: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable c = c
    try
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let mutable i: int = 0
        while i < (String.length (lower)) do
            if c = (string (lower.[i])) then
                __ret <- string (upper.[i])
                raise Return
            i <- i + 1
        __ret <- c
        raise Return
        __ret
    with
        | Return -> __ret
let rec process_text (text: string) (break_key: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    let mutable break_key = break_key
    try
        let mutable chars: string array = [||]
        let mutable i: int = 0
        try
            while i < (String.length (text)) do
                try
                    let mutable c: string = to_upper (string (text.[i]))
                    let mutable j: int = 0
                    let mutable ok: bool = false
                    try
                        while j < (String.length (KEY_STRING)) do
                            try
                                if (string (KEY_STRING.[j])) = c then
                                    ok <- true
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if ok then
                        chars <- Array.append chars [|c|]
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        if (Seq.length (chars)) = 0 then
            __ret <- ""
            raise Return
        let last: string = _idx chars ((Seq.length (chars)) - 1)
        while ((((Seq.length (chars)) % break_key + break_key) % break_key)) <> 0 do
            chars <- Array.append chars [|last|]
        let mutable res: string = ""
        let mutable k: int = 0
        while k < (Seq.length (chars)) do
            res <- res + (_idx chars (k))
            k <- k + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec matrix_minor (m: int array array) (row: int) (col: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable m = m
    let mutable row = row
    let mutable col = col
    try
        let mutable res: int array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (m)) do
            if i <> row then
                let mutable r: int array = [||]
                let mutable j: int = 0
                while j < (Seq.length (_idx m (i))) do
                    if j <> col then
                        r <- Array.append r [|_idx (_idx m (i)) (j)|]
                    j <- j + 1
                res <- Array.append res [|r|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec determinant (m: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable m = m
    try
        let n: int = Seq.length (m)
        if n = 1 then
            __ret <- _idx (_idx m (0)) (0)
            raise Return
        if n = 2 then
            __ret <- ((_idx (_idx m (0)) (0)) * (_idx (_idx m (1)) (1))) - ((_idx (_idx m (0)) (1)) * (_idx (_idx m (1)) (0)))
            raise Return
        let mutable det: int = 0
        let mutable col: int = 0
        while col < n do
            let minor_mat: int array array = matrix_minor (m) (0) (col)
            let mutable sign: int = 1
            if (((col % 2 + 2) % 2)) = 1 then
                sign <- -1
            det <- det + ((sign * (_idx (_idx m (0)) (col))) * (determinant (minor_mat)))
            col <- col + 1
        __ret <- det
        raise Return
        __ret
    with
        | Return -> __ret
let rec cofactor_matrix (m: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable m = m
    try
        let n: int = Seq.length (m)
        let mutable res: int array array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < n do
                let minor_mat: int array array = matrix_minor (m) (i) (j)
                let det_minor: int = determinant (minor_mat)
                let mutable sign: int = 1
                if ((((i + j) % 2 + 2) % 2)) = 1 then
                    sign <- -1
                row <- Array.append row [|sign * det_minor|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec transpose (m: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable m = m
    try
        let rows: int = Seq.length (m)
        let cols: int = Seq.length (_idx m (0))
        let mutable res: int array array = [||]
        let mutable j: int = 0
        while j < cols do
            let mutable row: int array = [||]
            let mutable i: int = 0
            while i < rows do
                row <- Array.append row [|_idx (_idx m (i)) (j)|]
                i <- i + 1
            res <- Array.append res [|row|]
            j <- j + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec matrix_mod (m: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable m = m
    try
        let mutable res: int array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (m)) do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < (Seq.length (_idx m (i))) do
                row <- Array.append row [|mod36 (_idx (_idx m (i)) (j))|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec scalar_matrix_mult (s: int) (m: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable s = s
    let mutable m = m
    try
        let mutable res: int array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (m)) do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < (Seq.length (_idx m (i))) do
                row <- Array.append row [|mod36 (s * (_idx (_idx m (i)) (j)))|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec adjugate (m: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable m = m
    try
        let cof: int array array = cofactor_matrix (m)
        let n: int = Seq.length (cof)
        let mutable res: int array array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < n do
                row <- Array.append row [|_idx (_idx cof (j)) (i)|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec multiply_matrix_vector (m: int array array) (v: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable m = m
    let mutable v = v
    try
        let n: int = Seq.length (m)
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable sum: int = 0
            let mutable j: int = 0
            while j < n do
                sum <- sum + ((_idx (_idx m (i)) (j)) * (_idx v (j)))
                j <- j + 1
            res <- Array.append res [|mod36 (sum)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec inverse_key (key: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable key = key
    try
        let det_val: int = determinant (key)
        let mutable det_mod: int = mod36 (det_val)
        let mutable det_inv: int = 0
        let mutable i: int = 0
        try
            while i < 36 do
                try
                    if ((((det_mod * i) % 36 + 36) % 36)) = 1 then
                        det_inv <- i
                        raise Break
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let adj: int array array = adjugate (key)
        let tmp: int array array = scalar_matrix_mult (det_inv) (adj)
        let mutable res: int array array = matrix_mod (tmp)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec hill_encrypt (key: int array array) (text: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    let mutable text = text
    try
        let break_key: int = Seq.length (key)
        let processed: string = process_text (text) (break_key)
        let mutable encrypted: string = ""
        let mutable i: int = 0
        while i < (String.length (processed)) do
            let mutable vec: int array = [||]
            let mutable j: int = 0
            while j < break_key do
                vec <- Array.append vec [|replace_letters (string (processed.[i + j]))|]
                j <- j + 1
            let enc_vec: int array = multiply_matrix_vector (key) (vec)
            let mutable k: int = 0
            while k < break_key do
                encrypted <- encrypted + (replace_digits (_idx enc_vec (k)))
                k <- k + 1
            i <- i + break_key
        __ret <- encrypted
        raise Return
        __ret
    with
        | Return -> __ret
let rec hill_decrypt (key: int array array) (text: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable key = key
    let mutable text = text
    try
        let break_key: int = Seq.length (key)
        let decrypt_key: int array array = inverse_key (key)
        let processed: string = process_text (text) (break_key)
        let mutable decrypted: string = ""
        let mutable i: int = 0
        while i < (String.length (processed)) do
            let mutable vec: int array = [||]
            let mutable j: int = 0
            while j < break_key do
                vec <- Array.append vec [|replace_letters (string (processed.[i + j]))|]
                j <- j + 1
            let dec_vec: int array = multiply_matrix_vector (decrypt_key) (vec)
            let mutable k: int = 0
            while k < break_key do
                decrypted <- decrypted + (replace_digits (_idx dec_vec (k)))
                k <- k + 1
            i <- i + break_key
        __ret <- decrypted
        raise Return
        __ret
    with
        | Return -> __ret
let key: int array array = [|[|2; 5|]; [|1; 6|]|]
printfn "%s" (hill_encrypt (key) ("testing hill cipher"))
printfn "%s" (hill_encrypt (key) ("hello"))
printfn "%s" (hill_decrypt (key) ("WHXYJOLM9C6XT085LL"))
printfn "%s" (hill_decrypt (key) ("85FF00"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
