{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
var
  zeroval_x: integer;
  main_i: integer;
  main_tmp: integer;
  main_box: array of integer;
function zeroval(ival: integer): integer;
begin
  zeroval_x := ival;
  zeroval_x := 0;
  exit(zeroval_x);
end;
procedure zeroptr(ref: IntArray);
begin
  ref[0] := 0;
end;
procedure main();
begin
  main_i := 1;
  writeln('initial: ' + IntToStr(main_i));
  main_tmp := zeroval(main_i);
  writeln('zeroval: ' + IntToStr(main_i));
  main_box := [main_i];
  zeroptr(main_box);
  main_i := main_box[0];
  writeln('zeroptr: ' + IntToStr(main_i));
  writeln('pointer: 0');
end;
begin
  main();
end.
