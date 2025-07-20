{$mode objfpc}
program Main;
uses SysUtils;
var
  s: string;
begin
  s := 'catch';
  writeln(ord(Pos('cat', s) <> 0));
  writeln(ord(Pos('dog', s) <> 0));
end.
