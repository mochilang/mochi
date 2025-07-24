{$mode objfpc}
program Main;
uses SysUtils;
var
  main_i: integer;
function bottles(n: integer): string;
begin
  if n = 0 then begin
  exit('No more bottles');
end;
  if n = 1 then begin
  exit('1 bottle');
end;
  exit(IntToStr(n) + ' bottles');
end;
procedure main();
begin
  main_i := 99;
  while main_i > 0 do begin
  writeln(bottles(main_i) + ' of beer on the wall');
  writeln(bottles(main_i) + ' of beer');
  writeln('Take one down, pass it around');
  writeln(bottles(main_i - 1) + ' of beer on the wall');
  main_i := main_i - 1;
end;
end;
begin
  main();
end.
