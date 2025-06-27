program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type 
  generic TArray<T> = array of T;

function findMedianSortedArrays(nums1: specialize TArray<integer>; nums2: specialize TArray<integer>
): double;

var 
  i: integer;
  j: integer;
  merged: specialize TArray<integer>;
  mid1: integer;
  mid2: integer;
  total: integer;
begin
  merged := specialize TArray<integer>([]);
  i := 0;
  j := 0;
  while ((i < Length(nums1)) or (j < Length(nums2))) do
    begin
      if (j >= Length(nums2)) then
        begin
          merged := Concat(merged, specialize TArray<integer>([specialize _indexList<integer>(nums1,
                    i)]));
          i := i + 1;
        end
      else if (i >= Length(nums1)) then
             begin
               merged := Concat(merged, specialize TArray<integer>([specialize _indexList<integer>(
                         nums2, j)]));
               j := j + 1;
             end
      else if (specialize _indexList<integer>(nums1, i) <= specialize _indexList<integer>(nums2, j))
             then
             begin
               merged := Concat(merged, specialize TArray<integer>([specialize _indexList<integer>(
                         nums1, i)]));
               i := i + 1;
             end
      else
        begin
          merged := Concat(merged, specialize TArray<integer>([specialize _indexList<integer>(nums2,
                    j)]));
          j := j + 1;
        end;
    end;
  total := Length(merged);
  if (total mod 2 = 1) then
    begin
      result := Double(merged[total div 2]);
      exit;
    end;
  mid1 := merged[total div 2 - 1];
  mid2 := merged[total div 2];
  result := Double(mid1 + mid2) / 2;
  exit;
end;

procedure test_example_1;
begin
  if not ((findMedianSortedArrays(specialize TArray<integer>([1, 3]), specialize TArray<integer>([2]
     )) = 2)) then raise Exception.Create('expect failed');
end;

procedure test_example_2;
begin
  if not ((findMedianSortedArrays(specialize TArray<integer>([1, 2]), specialize TArray<integer>([3,
     4])) = 2.5)) then raise Exception.Create('expect failed');
end;

procedure test_empty_first;
begin
  if not ((findMedianSortedArrays(Trunc(specialize TArray<integer>([])), specialize TArray<integer>(
     [1])) = 1)) then raise Exception.Create('expect failed');
end;

procedure test_empty_second;
begin
  if not ((findMedianSortedArrays(specialize TArray<integer>([2]), Trunc(specialize TArray<integer>(
     []))) = 2)) then raise Exception.Create('expect failed');
end;

generic function _indexList<T>(arr: specialize TArray<T>; i: integer): T;
begin
  if i < 0 then i := Length(arr) + i;
  if (i < 0) or (i >= Length(arr)) then
    raise Exception.Create('index out of range');
  Result := arr[i];
end;

begin
  test_example_1;
  test_example_2;
  test_empty_first;
  test_empty_second;
end.
