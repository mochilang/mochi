{$mode objfpc}
program Main;
uses SysUtils;
var
  pow_result: integer;
  pow_i: integer;
function pow(base: integer; exp: integer): integer; forward;
function ackermann2(m: integer; n: integer): integer; forward;
procedure main(); forward;
function pow(base: integer; exp: integer): integer;
begin
  pow_result := 1;
  pow_i := 0;
  while pow_i < exp do begin
  pow_result := pow_result * base;
  pow_i := pow_i + 1;
end;
  exit(pow_result);
end;
function ackermann2(m: integer; n: integer): integer;
begin
  if m = 0 then begin
  exit(n + 1);
end;
  if m = 1 then begin
  exit(n + 2);
end;
  if m = 2 then begin
  exit((2 * n) + 3);
end;
  if m = 3 then begin
  exit((8 * pow(2, n)) - 3);
end;
  if n = 0 then begin
  exit(ackermann2(m - 1, 1));
end;
  exit(ackermann2(m - 1, ackermann2(m, n - 1)));
end;
procedure main();
begin
  writeln('A(0, 0) = ' + IntToStr(ackermann2(0, 0)));
  writeln('A(1, 2) = ' + IntToStr(ackermann2(1, 2)));
  writeln('A(2, 4) = ' + IntToStr(ackermann2(2, 4)));
  writeln('A(3, 4) = ' + IntToStr(ackermann2(3, 4)));
end;
begin
  main();
end.
