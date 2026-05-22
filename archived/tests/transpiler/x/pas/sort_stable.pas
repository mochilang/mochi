{$mode objfpc}
program Main;
type Anon60 = record
  n: integer;
  v: string;
end;
var
  items: array of Anon60;
  result: array of string;
  i: Anon60;
begin
  items := [(n: 1; v: 'a'), (n: 1; v: 'b'), (n: 2; v: 'c')];
  result := [];
  for i in items do begin
  result := concat(result, [i.v]);
end;
  writeln(result);
end.
