program main;
{$mode objfpc}
uses SysUtils, fgl;

type
	generic TArray<T> = array of T;

function findMedianSortedArrays(nums1: specialize TArray<integer>; nums2: specialize TArray<integer>): double;
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
			merged := Concat(merged, specialize TArray<integer>([nums1[i]]));
			i := i + 1;
		end else if (i >= Length(nums1)) then
		begin
			merged := Concat(merged, specialize TArray<integer>([nums2[j]]));
			j := j + 1;
		end else if (nums1[i] <= nums2[j]) then
		begin
			merged := Concat(merged, specialize TArray<integer>([nums1[i]]));
			i := i + 1;
		end else
		begin
			merged := Concat(merged, specialize TArray<integer>([nums2[j]]));
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

begin
end.
