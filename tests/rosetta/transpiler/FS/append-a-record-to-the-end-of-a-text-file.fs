// Generated 2025-07-26 05:05 +0700

exception Return

let rec writeTwo () =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    try
        __ret <- [|"jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash"; "jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jsmith:/bin/bash"|]
        raise Return
        __ret
    with
        | Return -> __ret
and appendOneMore (lines: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable lines = lines
    try
        __ret <- Array.append lines [|"xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash"|]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable lines: string array = writeTwo()
        lines <- appendOneMore lines
        if ((int (Array.length lines)) >= 3) && ((unbox<string> (lines.[2])) = "xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash") then
            printfn "%s" "append okay"
        else
            printfn "%s" "it didn't work"
        __ret
    with
        | Return -> __ret
main()
