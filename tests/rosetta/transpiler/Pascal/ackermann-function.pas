{$mode objfpc}
program Main;
uses SysUtils;
function ackermann(m: integer; n: integer): integer; forward;
procedure main(); forward;
function ackermann(m: integer; n: integer): integer;
begin
  if m = 0 then begin
  exit(n + 1);
end;
  if n = 0 then begin
  exit(ackermann(m - 1, 1));
end;
  exit(ackermann(m - 1, ackermann(m, n - 1)));
end;
procedure main();
begin
  writeln('A(0, 0) = ' + IntToStr(ackermann(0, 0)));
  writeln('A(1, 2) = ' + IntToStr(ackermann(1, 2)));
  writeln('A(2, 4) = ' + IntToStr(ackermann(2, 4)));
  writeln('A(3, 4) = ' + IntToStr(ackermann(3, 4)));
end;
begin
  main();
end.
