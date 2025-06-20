open System

exception Return_isValid of bool
let isValid (s: string) : bool =
    try
        let mutable stack = [||]
        let n = s.Length
        for i = 0 to n - 1 do
            let c = (string s.[(if i < 0 then s.Length + i else i)])
            if (c = "(") then
                stack <- Array.append stack [|")"|]
            elif (c = "[") then
                stack <- Array.append stack [|"]"|]
            elif (c = "{") then
                stack <- Array.append stack [|"}"|]
            else
                if (stack.Length = 0) then
                    raise (Return_isValid (false))
                let top = stack.[(stack.Length - 1)]
                if (top <> c) then
                    raise (Return_isValid (false))
                stack <- stack.[0 .. ((stack.Length - 1) - 1)]
        raise (Return_isValid ((stack.Length = 0)))
        failwith "unreachable"
    with Return_isValid v -> v

