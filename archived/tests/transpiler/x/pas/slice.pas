{$mode objfpc}
program Main;
begin
  writeln(copy([1, 2, 3], 1, (3 - (1))));
  writeln(copy([1, 2, 3], 0, (2 - (0))));
  writeln(copy('hello', 1+1, (4 - (1))));
end.
