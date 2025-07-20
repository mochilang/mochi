{$mode objfpc}
program Main;
function boom(a: integer; b: integer): boolean;
begin
  writeln('boom');
  exit(true);
end;
begin
  writeln(ord(false and boom(1, 2)));
  writeln(ord(true or boom(1, 2)));
end.
