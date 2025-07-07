program ForListCollection;
var
  arr: array[1..3] of Integer = (1,2,3);
  i: Integer;
begin
  for i := 1 to 3 do
    Writeln(arr[i]);
end.
