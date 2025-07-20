{$mode objfpc}
program Main;
function min(xs: array of integer): integer;
var i, m: integer;
begin
  if Length(xs) = 0 then begin min := 0; exit; end;
  m := xs[0];
  for i := 1 to High(xs) do if xs[i] < m then m := xs[i];
  min := m;
end;
function max(xs: array of integer): integer;
var i, m: integer;
begin
  if Length(xs) = 0 then begin max := 0; exit; end;
  m := xs[0];
  for i := 1 to High(xs) do if xs[i] > m then m := xs[i];
  max := m;
end;
var
  nums: array of integer;
begin
  nums := [3, 1, 4];
  writeln(min(nums));
  writeln(max(nums));
end.
