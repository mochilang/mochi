{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Result = record
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  r1: Result;
  r2: Result;
  r3: Result;
  stress: real;
  tangential_force: real;
  area: real;
  r: Result;
function makeResult(name: string; value: real): Result; forward;
function shear_stress(stress: real; tangential_force: real; area: real): Result; forward;
function str_result(r: Result): string; forward;
function makeResult(name: string; value: real): Result;
begin
  Result.name := name;
  Result.value := value;
end;
function shear_stress(stress: real; tangential_force: real; area: real): Result;
var
  shear_stress_zeros: integer;
begin
  shear_stress_zeros := 0;
  if stress = 0 then begin
  shear_stress_zeros := shear_stress_zeros + 1;
end;
  if tangential_force = 0 then begin
  shear_stress_zeros := shear_stress_zeros + 1;
end;
  if area = 0 then begin
  shear_stress_zeros := shear_stress_zeros + 1;
end;
  if shear_stress_zeros <> 1 then begin
  panic('You cannot supply more or less than 2 values');
end else begin
  if stress < 0 then begin
  panic('Stress cannot be negative');
end else begin
  if tangential_force < 0 then begin
  panic('Tangential Force cannot be negative');
end else begin
  if area < 0 then begin
  panic('Area cannot be negative');
end else begin
  if stress = 0 then begin
  exit(makeResult('stress', tangential_force / area));
end else begin
  if tangential_force = 0 then begin
  exit(makeResult('tangential_force', stress * area));
end else begin
  exit(makeResult('area', tangential_force / stress));
end;
end;
end;
end;
end;
end;
end;
function str_result(r: Result): string;
begin
  exit(((('Result(name=''' + r.name) + ''', value=') + FloatToStr(r.value)) + ')');
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
end.
