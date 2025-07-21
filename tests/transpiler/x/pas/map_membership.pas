{$mode objfpc}
program Main;
type Anon49 = record
  a: integer;
  b: integer;
end;
var
  m: Anon49;
begin
  m := (a: 1; b: 2);
  writeln(ord(Pos('a', m) <> 0));
  writeln(ord(Pos('c', m) <> 0));
end.
