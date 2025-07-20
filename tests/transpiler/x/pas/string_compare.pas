{$mode objfpc}
program Main;
uses SysUtils;
begin
  writeln(ord('a' < 'b'));
  writeln(ord('a' <= 'a'));
  writeln(ord('b' > 'a'));
  writeln(ord('b' >= 'b'));
end.
