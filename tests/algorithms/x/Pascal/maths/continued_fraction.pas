{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
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
    _now := Integer(GetTickCount64()*1000);
  end;
end;
function _bench_now(): int64;
begin
  _bench_now := GetTickCount64()*1000;
end;
function _mem(): int64;
var h: TFPCHeapStatus;
begin
  h := GetFPCHeapStatus;
  _mem := h.CurrHeapUsed;
end;
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  lst: IntArray;
  b: integer;
  denominator: integer;
  numerator: integer;
  a: integer;
function floor_div(a: integer; b: integer): integer; forward;
function continued_fraction(numerator: integer; denominator: integer): IntArray; forward;
function list_to_string(lst: IntArray): string; forward;
function floor_div(a: integer; b: integer): integer;
var
  floor_div_q: integer;
  floor_div_r: integer;
begin
  floor_div_q := a div b;
  floor_div_r := a mod b;
  if (floor_div_r <> 0) and (((a < 0) and (b > 0)) or ((a > 0) and (b < 0))) then begin
  floor_div_q := floor_div_q - 1;
end;
  exit(floor_div_q);
end;
function continued_fraction(numerator: integer; denominator: integer): IntArray;
var
  continued_fraction_num: integer;
  continued_fraction_den: integer;
  continued_fraction_result_: array of integer;
  continued_fraction_integer_part: integer;
  continued_fraction_tmp: integer;
begin
  continued_fraction_num := numerator;
  continued_fraction_den := denominator;
  continued_fraction_result_ := [];
  while true do begin
  continued_fraction_integer_part := floor_div(continued_fraction_num, continued_fraction_den);
  continued_fraction_result_ := concat(continued_fraction_result_, IntArray([continued_fraction_integer_part]));
  continued_fraction_num := continued_fraction_num - (continued_fraction_integer_part * continued_fraction_den);
  if continued_fraction_num = 0 then begin
  break;
end;
  continued_fraction_tmp := continued_fraction_num;
  continued_fraction_num := continued_fraction_den;
  continued_fraction_den := continued_fraction_tmp;
end;
  exit(continued_fraction_result_);
end;
function list_to_string(lst: IntArray): string;
var
  list_to_string_s: string;
  list_to_string_i: integer;
begin
  list_to_string_s := '[';
  list_to_string_i := 0;
  while list_to_string_i < Length(lst) do begin
  list_to_string_s := list_to_string_s + IntToStr(lst[list_to_string_i]);
  if list_to_string_i < (Length(lst) - 1) then begin
  list_to_string_s := list_to_string_s + ', ';
end;
  list_to_string_i := list_to_string_i + 1;
end;
  exit(list_to_string_s + ']');
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln('Continued Fraction of 0.84375 is: ' + list_to_string(continued_fraction(27, 32)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
