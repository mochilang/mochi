{$mode objfpc}
program Main;
uses SysUtils;
function _input(): string;
var s: string;
begin
  if EOF(Input) then s := '' else ReadLn(s);
  _input := s;
end;
var
  main_a: integer;
  main_b: integer;
procedure main();
begin
  main_a := StrToInt(_input());
  main_b := StrToInt(_input());
  writeln(main_a + main_b);
end;
begin
  main();
end.
