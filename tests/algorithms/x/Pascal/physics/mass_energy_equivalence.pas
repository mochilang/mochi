{$mode objfpc}{$modeswitch nestedprocvars}
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
  C: real;
  mass: real;
  energy: real;
function energy_from_mass(mass: real): real; forward;
function mass_from_energy(energy: real): real; forward;
function energy_from_mass(mass: real): real;
begin
  if mass < 0 then begin
  panic('Mass can''t be negative.');
end;
  exit((mass * C) * C);
end;
function mass_from_energy(energy: real): real;
begin
  if energy < 0 then begin
  panic('Energy can''t be negative.');
end;
  exit(energy / (C * C));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  C := 2.99792458e+08;
  writeln(FloatToStr(energy_from_mass(124.56)));
  writeln(FloatToStr(energy_from_mass(320)));
  writeln(FloatToStr(energy_from_mass(0)));
  writeln(FloatToStr(mass_from_energy(124.56)));
  writeln(FloatToStr(mass_from_energy(320)));
  writeln(FloatToStr(mass_from_energy(0)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
