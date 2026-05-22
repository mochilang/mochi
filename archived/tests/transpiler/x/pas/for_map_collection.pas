{$mode objfpc}
program Main;
type Anon9 = record
  a: integer;
  b: integer;
end;
var
  m: Anon9;
  k: integer;
begin
  m := (a: 1; b: 2);
  for k in m do begin
  writeln(k);
end;
end.
