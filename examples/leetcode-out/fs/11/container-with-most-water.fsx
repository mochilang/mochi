open System

exception Return_maxArea of int
let maxArea (height: int[]) : int =
    try
        let mutable left = 0
        let mutable right = (height.Length - 1)
        let mutable maxArea = 0
        while (left < right) do
            let width = (right - left)
            let mutable h = 0
            if (height.[left] < height.[right]) then
                h <- height.[left]
            else
                h <- height.[right]
            let area = (h * width)
            if (area > maxArea) then
                maxArea <- area
            if (height.[left] < height.[right]) then
                left <- (left + 1)
            else
                right <- (right - 1)
        raise (Return_maxArea (maxArea))
        failwith "unreachable"
    with Return_maxArea v -> v

