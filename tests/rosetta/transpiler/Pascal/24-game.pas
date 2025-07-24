{$mode objfpc}
program Main;
uses SysUtils;
var _nowSeed: int64 = 0;
var _nowSeeded: boolean = false;
procedure init_now();
var s: string; v: int64;
begin
  s := GetEnvironmentVariable('MOCHI_NOW_SEED');
  if s <> '' then begin
    Val(s, v);
    _nowSeed := v;
    _nowSeeded := true;
  end;
end;
function _now(): integer;
begin
  if _nowSeeded then begin
    _nowSeed := (_nowSeed * 1664525 + 1013904223) mod 2147483647;
    _now := _nowSeed;
  end else begin
    _now := Integer(GetTickCount64());
  end;
end;
function _input(): string;
var s: string;
begin
  if EOF(Input) then s := '' else ReadLn(s);
  _input := s;
end;
var
  main_digits: array of boolean;
  i: integer;
  main_numstr: string;
  main_expr: string;
  main_stack: array of boolean;
  main_i: integer;
  main_valid: boolean;
  main_ch: string;
  main_j: integer;
  main_b: boolean;
  main_a: boolean;
function randDigit(): integer;
begin
  exit((_now() mod 9) + 1);
end;
procedure main();
begin
  main_digits := [];
  for i := 0 to (4 - 1) do begin
  main_digits := concat(main_digits, [randDigit()]);
end;
  main_numstr := '';
  for i := 0 to (4 - 1) do begin
  main_numstr := main_numstr + IntToStr(main_digits[i]);
end;
  writeln(('Your numbers: ' + main_numstr) + '' + #10 + '');
  writeln('Enter RPN: ');
  main_expr := _input();
  if Length(main_expr) <> 7 then begin
  writeln('invalid. expression length must be 7. (4 numbers, 3 operators, no spaces)');
  exit();
end;
  main_stack := [];
  main_i := 0;
  main_valid := true;
  while main_i < Length(main_expr) do begin
  main_ch := copy(main_expr, main_i+1, (main_i + 1 - (main_i)));
  if (main_ch >= '0') and (main_ch <= '9') then begin
  if Length(main_digits) = 0 then begin
  writeln('too many numbers.');
  exit();
end;
  main_j := 0;
  while main_digits[main_j] <> (int(main_ch) - int('0')) do begin
  main_j := main_j + 1;
  if main_j = Length(main_digits) then begin
  writeln('wrong numbers.');
  exit();
end;
end;
  main_digits := copy(main_digits, 0, main_j)) + copy(main_digits, main_j + 1, Length(main_digits));
  main_stack := concat(main_stack, [float(int(main_ch) - int('0'))]);
end else begin
  if Length(main_stack) < 2 then begin
  writeln('invalid expression syntax.');
  main_valid := false;
  break;
end;
  main_b := main_stack[Length(main_stack) - 1];
  main_a := main_stack[Length(main_stack) - 2];
  if main_ch = '+' then begin
  main_stack[Length(main_stack) - 2] := main_a + main_b;
end;
  main_stack := copy(main_stack, 0, Length(main_stack) - 1));
end;
  main_i := main_i + 1;
end;
  if main_valid then begin
  if abs(main_stack[0] - 24) > 1e-06 then begin
  writeln(('incorrect. ' + IntToStr(main_stack[0])) + ' != 24');
end else begin
  writeln('correct.');
end;
end;
end;
begin
  init_now();
  main();
end.
