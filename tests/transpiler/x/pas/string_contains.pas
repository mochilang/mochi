{$mode objfpc}
program Main;
var
  s: string;
begin
  s := 'catch';
  writeln(ord(Pos('cat', s) <> 0));
  writeln(ord(Pos('dog', s) <> 0));
end.
