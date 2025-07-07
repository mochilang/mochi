program AppendBuiltin;
var
  a: array of integer;
  i: integer;
begin
  SetLength(a, 2);
  a[0] := 1;
  a[1] := 2;
  SetLength(a, 3);
  a[2] := 3;
  for i := 0 to High(a) do
  begin
    if i > 0 then Write(' ');
    Write(a[i]);
  end;
  Writeln;
end.
