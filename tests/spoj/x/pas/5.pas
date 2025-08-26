{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math, fgl;
type IntArray = array of int64;
function _input(): string;
var s: string;
begin
  if EOF(Input) then s := '' else ReadLn(s);
  _input := s;
end;
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
procedure error(msg: string);
begin
  panic(msg);
end;
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: int64): real;
begin
  _to_float := x;
end;
function to_float(x: int64): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
function Map1(): specialize TFPGMap<string, int64>; forward;
function next_pal(next_pal_s: string): string; forward;
function parseIntStr(parseIntStr_str: string): integer; forward;
procedure main(); forward;
function Map1(): specialize TFPGMap<string, int64>;
begin
  Result := specialize TFPGMap<string, int64>.Create();
  Result.AddOrSetData('0', 0);
  Result.AddOrSetData('1', 1);
  Result.AddOrSetData('2', 2);
  Result.AddOrSetData('3', 3);
  Result.AddOrSetData('4', 4);
  Result.AddOrSetData('5', 5);
  Result.AddOrSetData('6', 6);
  Result.AddOrSetData('7', 7);
  Result.AddOrSetData('8', 8);
  Result.AddOrSetData('9', 9);
end;
function next_pal(next_pal_s: string): string;
var
  next_pal_digitMap: specialize TFPGMap<string, int64>;
  next_pal_n: integer;
  next_pal_num: array of int64;
  next_pal_i: int64;
  next_pal_all9: boolean;
  next_pal_d: int64;
  next_pal_res: string;
  next_pal__: int64;
  next_pal_left: integer;
  next_pal_right: integer;
  next_pal_smaller: boolean;
  next_pal_carry: int64;
  next_pal_mid: integer;
  next_pal_out_: string;
begin
  next_pal_digitMap := Map1();
  next_pal_n := Length(next_pal_s);
  next_pal_num := [];
  for next_pal_i := 0 to (next_pal_n - 1) do begin
  next_pal_num := concat(next_pal_num, IntArray([Trunc(next_pal_digitMap[copy(next_pal_s, next_pal_i+1, (next_pal_i + 1 - (next_pal_i)))])]));
end;
  next_pal_all9 := true;
  for next_pal_d in next_pal_num do begin
  if next_pal_d <> 9 then begin
  next_pal_all9 := false;
  break;
end;
end;
  if next_pal_all9 then begin
  next_pal_res := '1';
  for next_pal__ := 0 to (next_pal_n - 1 - 1) do begin
  next_pal_res := next_pal_res + '0';
end;
  next_pal_res := next_pal_res + '1';
  exit(next_pal_res);
end;
  next_pal_left := (_floordiv(next_pal_n, 2)) - 1;
  next_pal_right := IfThen((next_pal_n mod 2) = 0, _floordiv(next_pal_n, 2), (_floordiv(next_pal_n, 2)) + 1);
  while ((next_pal_left >= 0) and (next_pal_right < next_pal_n)) and (next_pal_num[next_pal_left] = next_pal_num[next_pal_right]) do begin
  next_pal_left := next_pal_left - 1;
  next_pal_right := next_pal_right + 1;
end;
  next_pal_smaller := (next_pal_left < 0) or (next_pal_num[next_pal_left] < next_pal_num[next_pal_right]);
  next_pal_left := (_floordiv(next_pal_n, 2)) - 1;
  if (next_pal_n mod 2) = 0 then begin
  next_pal_right := _floordiv(next_pal_n, 2);
end else begin
  next_pal_right := (_floordiv(next_pal_n, 2)) + 1;
end;
  while next_pal_left >= 0 do begin
  next_pal_num[next_pal_right] := next_pal_num[next_pal_left];
  next_pal_left := next_pal_left - 1;
  next_pal_right := next_pal_right + 1;
end;
  if next_pal_smaller then begin
  next_pal_carry := 1;
  next_pal_left := (_floordiv(next_pal_n, 2)) - 1;
  if (next_pal_n mod 2) = 1 then begin
  next_pal_mid := _floordiv(next_pal_n, 2);
  next_pal_num[next_pal_mid] := next_pal_num[next_pal_mid] + next_pal_carry;
  next_pal_carry := _floordiv(next_pal_num[next_pal_mid], 10);
  next_pal_num[next_pal_mid] := next_pal_num[next_pal_mid] mod 10;
  next_pal_right := next_pal_mid + 1;
end else begin
  next_pal_right := _floordiv(next_pal_n, 2);
end;
  while next_pal_left >= 0 do begin
  next_pal_num[next_pal_left] := next_pal_num[next_pal_left] + next_pal_carry;
  next_pal_carry := _floordiv(next_pal_num[next_pal_left], 10);
  next_pal_num[next_pal_left] := next_pal_num[next_pal_left] mod 10;
  next_pal_num[next_pal_right] := next_pal_num[next_pal_left];
  next_pal_left := next_pal_left - 1;
  next_pal_right := next_pal_right + 1;
end;
end;
  next_pal_out_ := '';
  for next_pal_d in next_pal_num do begin
  next_pal_out_ := next_pal_out_ + IntToStr(next_pal_d);
end;
  exit(next_pal_out_);
end;
function parseIntStr(parseIntStr_str: string): integer;
var
  parseIntStr_digits: specialize TFPGMap<string, int64>;
  parseIntStr_i: int64;
  parseIntStr_n: int64;
begin
  exit(StrToInt(parseIntStr_str));
end;
procedure main();
var
  main_tStr: string;
  main_t: integer;
  main__: int64;
  main_s: string;
begin
  main_tStr := _input();
  if main_tStr = '' then begin
  exit();
end;
  main_t := parseIntStr(main_tStr);
  for main__ := 0 to (main_t - 1) do begin
  main_s := _input();
  writeln(next_pal(main_s));
end;
end;
begin
  main();
end.
