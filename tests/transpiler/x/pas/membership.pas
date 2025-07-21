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
  nums: array of integer;
begin
  nums := [1, 2, 3];
  writeln(ord(contains(nums, 2)));
  writeln(ord(contains(nums, 4)));
end.
