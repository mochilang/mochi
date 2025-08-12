{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
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
  PLANCK_CONSTANT_JS: real;
  PLANCK_CONSTANT_EVS: real;
  exp: integer;
  frequency: real;
  in_ev: boolean;
  work_function: real;
function pow10(exp: integer): real; forward;
function maximum_kinetic_energy(frequency: real; work_function: real; in_ev: boolean): real; forward;
function pow10(exp: integer): real;
var
  pow10_result_: real;
  pow10_i: integer;
begin
  pow10_result_ := 1;
  pow10_i := 0;
  while pow10_i < exp do begin
  pow10_result_ := pow10_result_ * 10;
  pow10_i := pow10_i + 1;
end;
  exit(pow10_result_);
end;
function maximum_kinetic_energy(frequency: real; work_function: real; in_ev: boolean): real;
var
  maximum_kinetic_energy_energy: real;
begin
  if frequency < 0 then begin
  panic('Frequency can''t be negative.');
end;
  if in_ev then begin
  maximum_kinetic_energy_energy := (PLANCK_CONSTANT_EVS * frequency) - work_function;
end else begin
  maximum_kinetic_energy_energy := (PLANCK_CONSTANT_JS * frequency) - work_function;
end;
  if maximum_kinetic_energy_energy > 0 then begin
  exit(maximum_kinetic_energy_energy);
end;
  exit(0);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PLANCK_CONSTANT_JS := 6.6261 / pow10(34);
  PLANCK_CONSTANT_EVS := 4.1357 / pow10(15);
  writeln(FloatToStr(maximum_kinetic_energy(1e+06, 2, false)));
  writeln(FloatToStr(maximum_kinetic_energy(1e+06, 2, true)));
  writeln(FloatToStr(maximum_kinetic_energy(1e+16, 2, true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
