// Generated 2025-07-26 04:38 +0700

exception Break
exception Continue

exception Return

let rec sameDigits (n: int) (b: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    let mutable b = b
    try
        let mutable f: int = ((n % b + b) % b)
        n <- int (n / b)
        while n > 0 do
            if (((n % b + b) % b)) <> f then
                __ret <- false
                raise Return
            n <- int (n / b)
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and isBrazilian (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 7 then
            __ret <- false
            raise Return
        if ((((n % 2 + 2) % 2)) = 0) && (n >= 8) then
            __ret <- true
            raise Return
        let mutable b: int = 2
        while b < (n - 1) do
            if unbox<bool> (sameDigits n b) then
                __ret <- true
                raise Return
            b <- b + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and isPrime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 2 then
            __ret <- false
            raise Return
        if (((n % 2 + 2) % 2)) = 0 then
            __ret <- n = 2
            raise Return
        if (((n % 3 + 3) % 3)) = 0 then
            __ret <- n = 3
            raise Return
        let mutable d: int = 5
        while (d * d) <= n do
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 2
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 4
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable kinds: string array = [|" "; " odd "; " prime "|]
        for kind in kinds do
            try
                printfn "%s" (("First 20" + (unbox<string> kind)) + "Brazilian numbers:")
                let mutable c: int = 0
                let mutable n: int = 7
                try
                    while true do
                        if unbox<bool> (isBrazilian n) then
                            printfn "%s" ((string n) + " ")
                            c <- c + 1
                            if c = 20 then
                                printfn "%s" "\n"
                                raise Break
                        if (unbox<string> kind) = " " then
                            n <- n + 1
                        else
                            if (unbox<string> kind) = " odd " then
                                n <- n + 2
                            else
                                try
                                    while true do
                                        n <- n + 2
                                        if unbox<bool> (isPrime n) then
                                            raise Break
                                with
                                | Break -> ()
                                | Continue -> ()
                with
                | Break -> ()
                | Continue -> ()
            with
            | Break -> ()
            | Continue -> ()
        let mutable n: int = 7
        let mutable c: int = 0
        while c < 100000 do
            if unbox<bool> (isBrazilian n) then
                c <- c + 1
            n <- n + 1
        printfn "%s" ("The 100,000th Brazilian number: " + (string (n - 1)))
        __ret
    with
        | Return -> __ret
main()
