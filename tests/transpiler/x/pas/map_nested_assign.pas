{$mode objfpc}
program Main;
type Anon50 = record
  inner: integer;
end;
type Anon51 = record
  outer: Anon50;
end;
var
  data: Anon51;
begin
  data := (outer: (inner: 1));
  data['outer']['inner'] := 2;
  writeln(data['outer']['inner']);
end.
