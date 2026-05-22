{$mode objfpc}
program Main;
type Counter = record
  n: integer;
end;
procedure inc(c: Counter);
begin
  c.n := c.n + 1;
end;
var
  c: Counter;
begin
  c.n := 0;
  inc(c);
  writeln(c.n);
end.
