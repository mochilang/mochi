{$mode objfpc}
program Main;
type Anon4 = record
  n: integer;
  l: string;
end;
var
  nums: array of integer;
  letters: array of string;
  pairs: array of Anon4;
  n: integer;
  l: string;
  p: integer;
begin
  nums := [1, 2, 3];
  letters := ['A', 'B'];
  pairs := [];
  for n in nums do begin
  for l in letters do begin
  if (n mod 2) = 0 then begin
  pairs := concat(pairs, [(n: n; l: l)]);
end;
end;
end;
  writeln('--- Even pairs ---');
  for p in pairs do begin
  writeln(p.n, p.l);
end;
end.
