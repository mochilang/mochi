program InOperator;
var
  xs: array[1..3] of Integer = (1,2,3);
  i: Integer;
  found: Boolean;

begin
  found := False;
  for i := 1 to 3 do
    if xs[i] = 2 then found := True;
  Writeln(found);

  found := False;
  for i := 1 to 3 do
    if xs[i] = 5 then found := True;
  Writeln(not found);
end.
