open System

exception Return_findMedianSortedArrays of float
let findMedianSortedArrays (nums1: int[]) (nums2: int[]) : float =
    try
        let mutable merged = [||]
        let mutable i = 0
        let mutable j = 0
        while ((i < nums1.Length) || (j < nums2.Length)) do
            if (j >= nums2.Length) then
                merged <- Array.append merged [|nums1.[i]|]
                i <- (i + 1)
            elif (i >= nums1.Length) then
                merged <- Array.append merged [|nums2.[j]|]
                j <- (j + 1)
            elif (nums1.[i] <= nums2.[j]) then
                merged <- Array.append merged [|nums1.[i]|]
                i <- (i + 1)
            else
                merged <- Array.append merged [|nums2.[j]|]
                j <- (j + 1)
        let total = merged.Length
        if ((total % 2) = 1) then
            raise (Return_findMedianSortedArrays ((float merged.[(total / 2)])))
        let mid1 = merged.[((total / 2) - 1)]
        let mid2 = merged.[(total / 2)]
        raise (Return_findMedianSortedArrays (((float ((mid1 + mid2))) / 2.0)))
        failwith "unreachable"
    with Return_findMedianSortedArrays v -> v

