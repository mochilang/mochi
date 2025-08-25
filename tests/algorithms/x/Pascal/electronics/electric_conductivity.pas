{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Result_ = record
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
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
  ELECTRON_CHARGE: real;
  r1: Result_;
  r2: Result_;
  r3: Result_;
function makeResult_(kind: string; value: real): Result_; forward;
function electric_conductivity(electric_conductivity_conductivity: real; electric_conductivity_electron_conc: real; electric_conductivity_mobility: real): Result_; forward;
function makeResult_(kind: string; value: real): Result_;
begin
  Result.kind := kind;
  Result.value := value;
end;
function electric_conductivity(electric_conductivity_conductivity: real; electric_conductivity_electron_conc: real; electric_conductivity_mobility: real): Result_;
var
  electric_conductivity_zero_count: int64;
begin
  electric_conductivity_zero_count := 0;
  if electric_conductivity_conductivity = 0 then begin
  electric_conductivity_zero_count := electric_conductivity_zero_count + 1;
end;
  if electric_conductivity_electron_conc = 0 then begin
  electric_conductivity_zero_count := electric_conductivity_zero_count + 1;
end;
  if electric_conductivity_mobility = 0 then begin
  electric_conductivity_zero_count := electric_conductivity_zero_count + 1;
end;
  if electric_conductivity_zero_count <> 1 then begin
  panic('You cannot supply more or less than 2 values');
end;
  if electric_conductivity_conductivity < 0 then begin
  panic('Conductivity cannot be negative');
end;
  if electric_conductivity_electron_conc < 0 then begin
  panic('Electron concentration cannot be negative');
end;
  if electric_conductivity_mobility < 0 then begin
  panic('mobility cannot be negative');
end;
  if electric_conductivity_conductivity = 0 then begin
  exit(makeResult_('conductivity', (electric_conductivity_mobility * electric_conductivity_electron_conc) * ELECTRON_CHARGE));
end;
  if electric_conductivity_electron_conc = 0 then begin
  exit(makeResult_('electron_conc', electric_conductivity_conductivity / (electric_conductivity_mobility * ELECTRON_CHARGE)));
end;
  exit(makeResult_('mobility', electric_conductivity_conductivity / (electric_conductivity_electron_conc * ELECTRON_CHARGE)));
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
  writeln('');
end.
