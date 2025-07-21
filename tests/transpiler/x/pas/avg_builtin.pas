{$mode objfpc}
program Main;
function avg(xs: array of integer): real;
var i, s: integer;
begin
  if Length(xs) = 0 then begin avg := 0; exit; end;
  s := 0;
  for i := 0 to High(xs) do s := s + xs[i];
  avg := s / Length(xs);
end;
begin
  writeln(avg([1, 2, 3]));
end.
