exception Return_0 of float
let rec findMedianSortedArrays nums1 nums2 =
  try
    let merged = ref [] in
    let i = ref 0 in
    let j = ref 0 in
    while !i < List.length nums1 || !j < List.length nums2 do
      if !j >= List.length nums2 then begin
        merged := !merged @ [(List.nth nums1 !i)];
        i := !i + 1;
      end else begin
        if !i >= List.length nums1 then begin
          merged := !merged @ [(List.nth nums2 !j)];
          j := !j + 1;
        end else begin
          if (List.nth nums1 !i) <= (List.nth nums2 !j) then begin
            merged := !merged @ [(List.nth nums1 !i)];
            i := !i + 1;
          end else begin
            merged := !merged @ [(List.nth nums2 !j)];
            j := !j + 1;
          end;
        end;
      end;
    done;
    let total = List.length !merged in
    if total mod 2 = 1 then begin
      raise (Return_0 ((float_of_int (List.nth !merged total / 2))))
    end;
    let mid1 = (List.nth !merged total / 2 - 1) in
    let mid2 = (List.nth !merged total / 2) in
    raise (Return_0 ((float_of_int (mid1 + mid2)) /. 2.))
  with Return_0 v -> v

