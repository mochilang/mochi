{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Result = record
  kind: string;
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
  GRAVITATIONAL_CONSTANT: real;
  r1: Result;
  r2: Result;
  r3: Result;
  r4: Result;
  x: real;
  distance: real;
  mass_2: real;
  mass_1: real;
  force: real;
function makeResult(kind: string; value: real): Result; forward;
function sqrtApprox(x: real): real; forward;
function gravitational_law(force: real; mass_1: real; mass_2: real; distance: real): Result; forward;
function makeResult(kind: string; value: real): Result;
begin
  Result.kind := kind;
  Result.value := value;
end;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  sqrtApprox_guess := x / 2;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function gravitational_law(force: real; mass_1: real; mass_2: real; distance: real): Result;
var
  gravitational_law_zero_count: integer;
  gravitational_law_product_of_mass: real;
  gravitational_law_f: real;
  gravitational_law_m1: real;
  gravitational_law_m2: real;
  gravitational_law_d: real;
begin
  gravitational_law_zero_count := 0;
  if force = 0 then begin
  gravitational_law_zero_count := gravitational_law_zero_count + 1;
end;
  if mass_1 = 0 then begin
  gravitational_law_zero_count := gravitational_law_zero_count + 1;
end;
  if mass_2 = 0 then begin
  gravitational_law_zero_count := gravitational_law_zero_count + 1;
end;
  if distance = 0 then begin
  gravitational_law_zero_count := gravitational_law_zero_count + 1;
end;
  if gravitational_law_zero_count <> 1 then begin
  panic('One and only one argument must be 0');
end;
  if force < 0 then begin
  panic('Gravitational force can not be negative');
end;
  if distance < 0 then begin
  panic('Distance can not be negative');
end;
  if mass_1 < 0 then begin
  panic('Mass can not be negative');
end;
  if mass_2 < 0 then begin
  panic('Mass can not be negative');
end;
  gravitational_law_product_of_mass := mass_1 * mass_2;
  if force = 0 then begin
  gravitational_law_f := (GRAVITATIONAL_CONSTANT * gravitational_law_product_of_mass) / (distance * distance);
  exit(makeResult('force', gravitational_law_f));
end;
  if mass_1 = 0 then begin
  gravitational_law_m1 := (force * (distance * distance)) / (GRAVITATIONAL_CONSTANT * mass_2);
  exit(makeResult('mass_1', gravitational_law_m1));
end;
  if mass_2 = 0 then begin
  gravitational_law_m2 := (force * (distance * distance)) / (GRAVITATIONAL_CONSTANT * mass_1);
  exit(makeResult('mass_2', gravitational_law_m2));
end;
  gravitational_law_d := sqrtApprox((GRAVITATIONAL_CONSTANT * gravitational_law_product_of_mass) / force);
  exit(makeResult('distance', gravitational_law_d));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  GRAVITATIONAL_CONSTANT := 6.6743e-11;
  r1 := gravitational_law(0, 5, 10, 20);
  r2 := gravitational_law(7367.382, 0, 74, 3048);
  r3 := gravitational_law(100, 5, 0, 3);
  r4 := gravitational_law(100, 5, 10, 0);
  writeln((r1.kind + ' ') + FloatToStr(r1.value));
  writeln((r2.kind + ' ') + FloatToStr(r2.value));
  writeln((r3.kind + ' ') + FloatToStr(r3.value));
  writeln((r4.kind + ' ') + FloatToStr(r4.value));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
