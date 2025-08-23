{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Result_ = record
  name: string;
  value: real;
end;
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
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  r1: Result_;
  r2: Result_;
  r3: Result_;
function makeResult_(name: string; value: real): Result_; forward;
function shear_stress(shear_stress_stress: real; shear_stress_tangential_force: real; shear_stress_area: real): Result_; forward;
function str_result(str_result_r: Result_): string; forward;
function makeResult_(name: string; value: real): Result_;
begin
  Result.name := name;
  Result.value := value;
end;
function shear_stress(shear_stress_stress: real; shear_stress_tangential_force: real; shear_stress_area: real): Result_;
var
  shear_stress_zeros: int64;
begin
  shear_stress_zeros := 0;
  if shear_stress_stress = 0 then begin
  shear_stress_zeros := shear_stress_zeros + 1;
end;
  if shear_stress_tangential_force = 0 then begin
  shear_stress_zeros := shear_stress_zeros + 1;
end;
  if shear_stress_area = 0 then begin
  shear_stress_zeros := shear_stress_zeros + 1;
end;
  if shear_stress_zeros <> 1 then begin
  panic('You cannot supply more or less than 2 values');
end else begin
  if shear_stress_stress < 0 then begin
  panic('Stress cannot be negative');
end else begin
  if shear_stress_tangential_force < 0 then begin
  panic('Tangential Force cannot be negative');
end else begin
  if shear_stress_area < 0 then begin
  panic('Area cannot be negative');
end else begin
  if shear_stress_stress = 0 then begin
  exit(makeResult_('stress', shear_stress_tangential_force / shear_stress_area));
end else begin
  if shear_stress_tangential_force = 0 then begin
  exit(makeResult_('tangential_force', shear_stress_stress * shear_stress_area));
end else begin
  exit(makeResult_('area', shear_stress_tangential_force / shear_stress_stress));
end;
end;
end;
end;
end;
end;
end;
function str_result(str_result_r: Result_): string;
begin
  exit(((('Result(name=''' + str_result_r.name) + ''', value=') + FloatToStr(str_result_r.value)) + ')');
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  r1 := shear_stress(25, 100, 0);
  writeln(str_result(r1));
  r2 := shear_stress(0, 1600, 200);
  writeln(str_result(r2));
  r3 := shear_stress(1000, 0, 1200);
  writeln(str_result(r3));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
