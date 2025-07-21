{$mode objfpc}
program Main;
type Anon46 = record
  alice: integer;
end;
var
  scores: Anon46;
begin
  scores := (alice: 1);
  scores['bob'] := 2;
  writeln(scores['bob']);
end.
