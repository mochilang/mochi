{$mode objfpc}
program Main;
type Anon5 = record
  n: integer;
  l: string;
  b: boolean;
end;
var
  nums: array of integer;
  letters: array of string;
  bools: array of boolean;
  combos: array of Anon5;
  c: integer;
  n: integer;
  l: string;
  b: boolean;
begin
  nums := [1, 2];
  letters := ['A', 'B'];
  bools := [true, false];
  combos := [];
  for n in nums do begin
  for l in letters do begin
  for b in bools do begin
  combos := concat(combos, [(n: n; l: l; b: b)]);
end;
end;
end;
  writeln('--- Cross Join of three lists ---');
  for c in combos do begin
  writeln(c.n, ' ', c.l, ' ', c.b);
end;
end.
