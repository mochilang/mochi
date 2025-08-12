{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
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
  x: real;
  p: RealArray;
  a: RealArray;
  b: RealArray;
function abs_val(x: real): real; forward;
procedure validate_point(p: RealArray); forward;
function manhattan_distance(a: RealArray; b: RealArray): real; forward;
function manhattan_distance_one_liner(a: RealArray; b: RealArray): real; forward;
function abs_val(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
procedure validate_point(p: RealArray);
begin
  if Length(p) = 0 then begin
  panic('Missing an input');
end;
end;
function manhattan_distance(a: RealArray; b: RealArray): real;
var
  manhattan_distance_total: real;
  manhattan_distance_i: integer;
begin
  validate_point(a);
  validate_point(b);
  if Length(a) <> Length(b) then begin
  panic('Both points must be in the same n-dimensional space');
end;
  manhattan_distance_total := 0;
  manhattan_distance_i := 0;
  while manhattan_distance_i < Length(a) do begin
  manhattan_distance_total := manhattan_distance_total + abs_val(a[manhattan_distance_i] - b[manhattan_distance_i]);
  manhattan_distance_i := manhattan_distance_i + 1;
end;
  exit(manhattan_distance_total);
end;
function manhattan_distance_one_liner(a: RealArray; b: RealArray): real;
begin
  exit(manhattan_distance(a, b));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(FloatToStr(manhattan_distance([1, 1], [2, 2])));
  writeln(FloatToStr(manhattan_distance([1.5, 1.5], [2, 2])));
  writeln(FloatToStr(manhattan_distance_one_liner([1.5, 1.5], [2.5, 2])));
  writeln(FloatToStr(manhattan_distance_one_liner([-3, -3, -3], [0, 0, 0])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
