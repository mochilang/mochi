program Slice;
var
  arr: array[0..2] of integer = (1,2,3);
  i: integer;
begin
  for i := 1 to 2 do
  begin
    if i > 1 then Write(' ');
    Write(arr[i]);
  end;
  Writeln;

  for i := 0 to 1 do
  begin
    if i > 0 then Write(' ');
    Write(arr[i]);
  end;
  Writeln;

  Writeln(Copy('hello',2,3));
end.
