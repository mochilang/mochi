open System

exception Return_dfs of bool

exception Return_isMatch of bool
let isMatch (s: string) (p: string) : bool =
    try
        let m = s.Length
        let n = p.Length
        let mutable memo = Map.ofList []
        let rec dfs (i: int) (j: int) : bool =
            try
                let key = ((i * ((n + 1))) + j)
                if Map.containsKey key memo then
                    raise (Return_dfs (memo.[key]))
                if (j = n) then
                    raise (Return_dfs ((i = m)))
                let mutable first = false
                if (i < m) then
                    if ((((string p.[(if j < 0 then p.Length + j else j)]) = (string s.[(if i < 0 then s.Length + i else i)]))) || (((string p.[(if j < 0 then p.Length + j else j)]) = "."))) then
                        first <- true
                let mutable ans = false
                if ((j + 1) < n) then
                    if ((string p.[(if (j + 1) < 0 then p.Length + (j + 1) else (j + 1))]) = "*") then
                        if dfs i (j + 2) then
                            ans <- true
                        elif (first && dfs (i + 1) j) then
                            ans <- true
                    else
                        if (first && dfs (i + 1) (j + 1)) then
                            ans <- true
                else
                    if (first && dfs (i + 1) (j + 1)) then
                        ans <- true
                memo <- Map.add key ans memo
                raise (Return_dfs (ans))
                failwith "unreachable"
            with Return_dfs v -> v
        raise (Return_isMatch (dfs 0 0))
        failwith "unreachable"
    with Return_isMatch v -> v

