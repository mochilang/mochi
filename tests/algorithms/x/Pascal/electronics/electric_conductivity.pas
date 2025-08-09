{$mode objfpc}
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
  ELECTRON_CHARGE: real;
  r1: Result;
  r2: Result;
  r3: Result;
  conductivity: real;
  electron_conc: real;
  mobility: real;
function makeResult(kind: string; value: real): Result; forward;
function electric_conductivity(conductivity: real; electron_conc: real; mobility: real): Result; forward;
function makeResult(kind: string; value: real): Result;
begin
  Result.kind := kind;
  Result.value := value;
end;
function electric_conductivity(conductivity: real; electron_conc: real; mobility: real): Result;
var
  electric_conductivity_zero_count: integer;
begin
  electric_conductivity_zero_count := 0;
  if conductivity = 0 then begin
  electric_conductivity_zero_count := electric_conductivity_zero_count + 1;
end;
  if electron_conc = 0 then begin
  electric_conductivity_zero_count := electric_conductivity_zero_count + 1;
end;
  if mobility = 0 then begin
  electric_conductivity_zero_count := electric_conductivity_zero_count + 1;
end;
  if electric_conductivity_zero_count <> 1 then begin
  panic('You cannot supply more or less than 2 values');
end;
  if conductivity < 0 then begin
  panic('Conductivity cannot be negative');
end;
  if electron_conc < 0 then begin
  panic('Electron concentration cannot be negative');
end;
  if mobility < 0 then begin
  panic('mobility cannot be negative');
end;
  if conductivity = 0 then begin
  exit(makeResult('conductivity', (mobility * electron_conc) * ELECTRON_CHARGE));
end;
  if electron_conc = 0 then begin
  exit(makeResult('electron_conc', conductivity / (mobility * ELECTRON_CHARGE)));
end;
  exit(makeResult('mobility', conductivity / (electron_conc * ELECTRON_CHARGE)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  ELECTRON_CHARGE := 1.6021e-19;
  r1 := electric_conductivity(25, 100, 0);
  r2 := electric_conductivity(0, 1600, 200);
  r3 := electric_conductivity(1000, 0, 1200);
  writeln((r1.kind + ' ') + FloatToStr(r1.value));
  writeln((r2.kind + ' ') + FloatToStr(r2.value));
  writeln((r3.kind + ' ') + FloatToStr(r3.value));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
