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
procedure error(msg: string);
begin
  panic(msg);
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
function list_real_to_str(xs: array of real): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + FloatToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  vector1: array of real;
  result1: RealArray;
  vector2: array of real;
  result2: RealArray;
function leaky_rectified_linear_unit(leaky_rectified_linear_unit_vector: RealArray; leaky_rectified_linear_unit_alpha: real): RealArray; forward;
function leaky_rectified_linear_unit(leaky_rectified_linear_unit_vector: RealArray; leaky_rectified_linear_unit_alpha: real): RealArray;
var
  leaky_rectified_linear_unit_result_: array of real;
  leaky_rectified_linear_unit_i: integer;
  leaky_rectified_linear_unit_x: real;
begin
  leaky_rectified_linear_unit_result_ := [];
  leaky_rectified_linear_unit_i := 0;
  while leaky_rectified_linear_unit_i < Length(leaky_rectified_linear_unit_vector) do begin
  leaky_rectified_linear_unit_x := leaky_rectified_linear_unit_vector[leaky_rectified_linear_unit_i];
  if leaky_rectified_linear_unit_x > 0 then begin
  leaky_rectified_linear_unit_result_ := concat(leaky_rectified_linear_unit_result_, [leaky_rectified_linear_unit_x]);
end else begin
  leaky_rectified_linear_unit_result_ := concat(leaky_rectified_linear_unit_result_, [leaky_rectified_linear_unit_alpha * leaky_rectified_linear_unit_x]);
end;
  leaky_rectified_linear_unit_i := leaky_rectified_linear_unit_i + 1;
end;
  exit(leaky_rectified_linear_unit_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  vector1 := [2.3, 0.6, -2, -3.8];
  result1 := leaky_rectified_linear_unit(vector1, 0.3);
  writeln(list_real_to_str(result1));
  vector2 := [-9.2, -0.3, 0.45, -4.56];
  result2 := leaky_rectified_linear_unit(vector2, 0.067);
  writeln(list_real_to_str(result2));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
