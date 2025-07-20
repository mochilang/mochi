{$mode objfpc}
program Main;
uses SysUtils;
function boom(): boolean;
begin
  writeln('boom');
  exit(true);
end;
begin
  writeln(ord(((1 < 2) and (2 < 3)) and (3 < 4)));
  writeln(ord(((1 < 2) and (2 > 3)) and boom()));
  writeln(ord((((1 < 2) and (2 < 3)) and (3 > 4)) and boom()));
end.
