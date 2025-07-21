{$mode objfpc}
program Main;
uses SysUtils;
var
  x: integer;
  msg: string;
begin
  x := 12;
  msg := IfThen(x > 10, 'yes', 'no');
  writeln(msg);
end.
