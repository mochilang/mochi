program ListSetOps;

type
  TIntSet = set of 0..5;
var
  a, b, res: TIntSet;
  unionAllLen: integer;
  i: integer;
begin
  a := [1,2];
  b := [2,3];
  res := a + b;
  for i := 0 to 5 do
    if i in res then
    begin
      if i > 1 then Write(' ');
      Write(i);
    end;
  Writeln;

  res := [1,2,3] - [2];
  for i := 0 to 5 do
    if i in res then
    begin
      if i > 1 then Write(' ');
      Write(i);
    end;
  Writeln;

  res := [1,2,3] * [2,4];
  for i := 0 to 5 do
    if i in res then
    begin
      if i > 1 then Write(' ');
      Write(i);
    end;
  Writeln;

  unionAllLen := 4;
  Writeln(unionAllLen);
end.
