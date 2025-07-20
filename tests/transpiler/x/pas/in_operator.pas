{$mode objfpc}
program Main;
function contains(xs: array of integer; v: integer): boolean;
var i: integer;
begin
  for i := 0 to High(xs) do begin
    if xs[i] = v then begin
      contains := true; exit;
    end;
  end;
  contains := false;
end;
var
  xs: array of integer;
begin
  xs := [1, 2, 3];
  writeln(ord(contains(xs, 2)));
  writeln(ord(not contains(xs, 5)));
end.
