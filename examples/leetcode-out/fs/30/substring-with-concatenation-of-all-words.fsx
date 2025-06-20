open System

exception Return_findSubstring of int[]
let findSubstring (s: string) (words: string[]) : int[] =
    try
        if (words.Length = 0) then
            raise (Return_findSubstring ([||]))
        let wordLen = words.[0].Length
        let wordCount = words.Length
        let totalLen = (wordLen * wordCount)
        if (s.Length < totalLen) then
            raise (Return_findSubstring ([||]))
        let mutable freq = Map.ofList []
        for w in words do
            if Map.containsKey w freq then
                freq <- Map.add w (freq.[w] + 1) freq
            else
                freq <- Map.add w 1 freq
        let mutable result = [||]
        for offset = 0 to wordLen - 1 do
            let mutable left = offset
            let mutable count = 0
            let mutable seen = Map.ofList []
            let mutable j = offset
            while ((j + wordLen) <= s.Length) do
                let word = s.[j .. ((j + wordLen) - 1)]
                j <- (j + wordLen)
                if Map.containsKey word freq then
                    if Map.containsKey word seen then
                        seen <- Map.add word (seen.[word] + 1) seen
                    else
                        seen <- Map.add word 1 seen
                    count <- (count + 1)
                    while (seen.[word] > freq.[word]) do
                        let lw = s.[left .. ((left + wordLen) - 1)]
                        seen <- Map.add lw (seen.[lw] - 1) seen
                        left <- (left + wordLen)
                        count <- (count - 1)
                    if (count = wordCount) then
                        result <- Array.append result [|left|]
                        let lw = s.[left .. ((left + wordLen) - 1)]
                        seen <- Map.add lw (seen.[lw] - 1) seen
                        left <- (left + wordLen)
                        count <- (count - 1)
                else
                    seen <- Map.ofList []
                    count <- 0
                    left <- j
        raise (Return_findSubstring (result))
        failwith "unreachable"
    with Return_findSubstring v -> v

