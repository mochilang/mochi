let data = dict [ ("outer", dict [("inner",1)]) ]
(data.["outer"]).["inner"] <- 2
printfn "%d" (data.["outer"].["inner"])
