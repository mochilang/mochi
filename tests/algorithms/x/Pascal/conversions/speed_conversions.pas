{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
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
  units: array of string;
  speed_chart: array of real;
  speed_chart_inverse: array of real;
  speed: real;
  unit_to: string;
  arr: StrArray;
  x: real;
  unit_from: string;
  value: string;
function index_of(arr: StrArray; value: string): integer; forward;
function units_string(arr: StrArray): string; forward;
function round3(x: real): real; forward;
function convert_speed(speed: real; unit_from: string; unit_to: string): real; forward;
function index_of(arr: StrArray; value: string): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(arr) do begin
  if arr[index_of_i] = value then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(-1);
end;
function units_string(arr: StrArray): string;
var
  units_string_s: string;
  units_string_i: integer;
begin
  units_string_s := '';
  units_string_i := 0;
  while units_string_i < Length(arr) do begin
  if units_string_i > 0 then begin
  units_string_s := units_string_s + ', ';
end;
  units_string_s := units_string_s + arr[units_string_i];
  units_string_i := units_string_i + 1;
end;
  exit(units_string_s);
end;
function round3(x: real): real;
var
  round3_y: real;
  round3_z: integer;
  round3_zf: real;
begin
  round3_y := (x * 1000) + 0.5;
  round3_z := Trunc(round3_y);
  round3_zf := Double(round3_z);
  exit(round3_zf / 1000);
end;
function convert_speed(speed: real; unit_from: string; unit_to: string): real;
var
  convert_speed_from_index: integer;
  convert_speed_to_index: integer;
  convert_speed_msg: string;
  convert_speed_result_: real;
  convert_speed_r: real;
begin
  convert_speed_from_index := index_of(units, unit_from);
  convert_speed_to_index := index_of(units, unit_to);
  if (convert_speed_from_index < 0) or (convert_speed_to_index < 0) then begin
  convert_speed_msg := (((('Incorrect ''from_type'' or ''to_type'' value: ' + unit_from) + ', ') + unit_to) + '' + #10 + 'Valid values are: ') + units_string(units);
  panic(convert_speed_msg);
end;
  convert_speed_result_ := (speed * speed_chart[convert_speed_from_index]) * speed_chart_inverse[convert_speed_to_index];
  convert_speed_r := round3(convert_speed_result_);
  exit(convert_speed_r);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  units := ['km/h', 'm/s', 'mph', 'knot'];
  speed_chart := [1, 3.6, 1.609344, 1.852];
  speed_chart_inverse := [1, 0.277777778, 0.621371192, 0.539956803];
  writeln(FloatToStr(convert_speed(100, 'km/h', 'm/s')));
  writeln(FloatToStr(convert_speed(100, 'km/h', 'mph')));
  writeln(FloatToStr(convert_speed(100, 'km/h', 'knot')));
  writeln(FloatToStr(convert_speed(100, 'm/s', 'km/h')));
  writeln(FloatToStr(convert_speed(100, 'm/s', 'mph')));
  writeln(FloatToStr(convert_speed(100, 'm/s', 'knot')));
  writeln(FloatToStr(convert_speed(100, 'mph', 'km/h')));
  writeln(FloatToStr(convert_speed(100, 'mph', 'm/s')));
  writeln(FloatToStr(convert_speed(100, 'mph', 'knot')));
  writeln(FloatToStr(convert_speed(100, 'knot', 'km/h')));
  writeln(FloatToStr(convert_speed(100, 'knot', 'm/s')));
  writeln(FloatToStr(convert_speed(100, 'knot', 'mph')));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
