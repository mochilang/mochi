{$mode objfpc}
program Main;
uses SysUtils;
var
  x: integer;
  msg: string;
begin
  x := 8;
  msg := IfThen(x > 10, 'big', IfThen(x > 5, 'medium', 'small'));
  writeln(msg);
end.
