// Generated 2025-08-01 18:27 +0700

exception Return

let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let mutable small: string array = [|"zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen"|]
let mutable tens: string array = [|""; ""; "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety"|]
let mutable illions: string array = [|""; " thousand"; " million"; " billion"; " trillion"; " quadrillion"; " quintillion"|]
let rec capitalize (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        __ret <- if (String.length s) = 0 then s else ((unbox<string> ((_substring s 0 1).ToUpper())) + (_substring s 1 (String.length s)))
        raise Return
        __ret
    with
        | Return -> __ret
and say (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable t: string = ""
        if n < 0 then
            t <- "negative "
            n <- -n
        if n < 20 then
            __ret <- t + (small.[n])
            raise Return
        else
            if n < 100 then
                t <- tens.[n / 10]
                let mutable s: int = ((n % 10 + 10) % 10)
                if s > 0 then
                    t <- (t + "-") + (small.[s])
                __ret <- t
                raise Return
            else
                if n < 1000 then
                    t <- (small.[n / 100]) + " hundred"
                    let mutable s: int = ((n % 100 + 100) % 100)
                    if s > 0 then
                        t <- (t + " ") + (unbox<string> (say s))
                    __ret <- t
                    raise Return
        let mutable sx: string = ""
        let mutable i: int = 0
        let mutable nn: int = n
        while nn > 0 do
            let p: int = ((nn % 1000 + 1000) % 1000)
            nn <- nn / 1000
            if p > 0 then
                let mutable ix: string = (unbox<string> (say p)) + (illions.[i])
                if sx <> "" then
                    ix <- (ix + " ") + sx
                sx <- ix
            i <- i + 1
        __ret <- t + sx
        raise Return
        __ret
    with
        | Return -> __ret
and fourIsMagic (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = say n
        s <- capitalize s
        let mutable t: string = s
        while n <> 4 do
            n <- String.length s
            s <- say n
            t <- (((t + " is ") + s) + ", ") + s
        t <- t + " is magic."
        __ret <- t
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let nums: int array = [|0; 4; 6; 11; 13; 75; 100; 337; -164; (int 9223372036854775807L)|]
        for n in nums do
            printfn "%s" (fourIsMagic n)
        __ret
    with
        | Return -> __ret
main()
