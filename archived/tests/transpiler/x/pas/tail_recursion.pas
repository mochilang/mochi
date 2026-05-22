{$mode objfpc}
program Main;
function sum_rec(n: integer; acc: integer): integer;
begin
  if n = 0 then begin
  exit(acc);
end;
  exit(sum_rec(n - 1, acc + n));
end;
begin
  writeln(sum_rec(10, 0));
end.
