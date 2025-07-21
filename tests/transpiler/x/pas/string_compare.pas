{$mode objfpc}
program Main;
begin
  writeln(ord('a' < 'b'));
  writeln(ord('a' <= 'a'));
  writeln(ord('b' > 'a'));
  writeln(ord('b' >= 'b'));
end.
