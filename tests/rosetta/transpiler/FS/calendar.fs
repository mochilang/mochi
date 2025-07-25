// Generated 2025-07-26 04:38 +0700

let daysInMonth: int array = [|31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31|]
let start: int array = [|3; 6; 6; 2; 4; 0; 2; 5; 1; 3; 6; 1|]
let months: string array = [|" January "; " February"; "  March  "; "  April  "; "   May   "; "   June  "; "   July  "; "  August "; "September"; " October "; " November"; " December"|]
let days: string array = [|"Su"; "Mo"; "Tu"; "We"; "Th"; "Fr"; "Sa"|]
printfn "%s" "                                [SNOOPY]\n"
printfn "%s" "                                  1969\n"
let mutable qtr: int = 0
while qtr < 4 do
    let mutable mi: int = 0
    while mi < 3 do
        printfn "%s" (String.concat " " [|sprintf "%A" (("      " + (unbox<string> (months.[(qtr * 3) + mi]))) + "           "); sprintf "%b" false|])
        mi <- mi + 1
    printfn "%s" ""
    mi <- 0
    while mi < 3 do
        let mutable d: int = 0
        while d < 7 do
            printfn "%s" (String.concat " " [|sprintf "%A" (" " + (unbox<string> (days.[d]))); sprintf "%b" false|])
            d <- d + 1
        printfn "%s" (String.concat " " [|sprintf "%A" "     "; sprintf "%b" false|])
        mi <- mi + 1
    printfn "%s" ""
    let mutable week: int = 0
    while week < 6 do
        mi <- 0
        while mi < 3 do
            let mutable day: int = 0
            while day < 7 do
                let m: int = (qtr * 3) + mi
                let ``val`` = (int (((week * 7) + day) - (int (start.[m])))) + 1
                if ((int ``val``) >= 1) && (``val`` <= (daysInMonth.[m])) then
                    let mutable s: string = string ``val``
                    if (String.length s) = 1 then
                        s <- " " + s
                    printfn "%s" (String.concat " " [|sprintf "%A" (" " + s); sprintf "%b" false|])
                else
                    printfn "%s" (String.concat " " [|sprintf "%A" "   "; sprintf "%b" false|])
                day <- day + 1
            printfn "%s" (String.concat " " [|sprintf "%A" "     "; sprintf "%b" false|])
            mi <- mi + 1
        printfn "%s" ""
        week <- week + 1
    printfn "%s" ""
    qtr <- qtr + 1
