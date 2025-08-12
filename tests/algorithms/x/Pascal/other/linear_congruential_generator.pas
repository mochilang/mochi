{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type LCG = record
  multiplier: integer;
  increment: integer;
  modulo: integer;
  seed: integer;
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
  lcg_var: LCG;
  i: integer;
  multiplier: integer;
  modulo: integer;
  increment: integer;
  seed: integer;
function makeLCG(multiplier: integer; increment: integer; modulo: integer; seed: integer): LCG; forward;
function make_lcg(multiplier: integer; increment: integer; modulo: integer; seed: integer): LCG; forward;
function next_number(lcg_var: LCG): integer; forward;
function makeLCG(multiplier: integer; increment: integer; modulo: integer; seed: integer): LCG;
begin
  Result.multiplier := multiplier;
  Result.increment := increment;
  Result.modulo := modulo;
  Result.seed := seed;
end;
function make_lcg(multiplier: integer; increment: integer; modulo: integer; seed: integer): LCG;
begin
  exit(makeLCG(multiplier, increment, modulo, seed));
end;
function next_number(lcg_var: LCG): integer;
begin
  lcg_var.seed := ((lcg_var.multiplier * lcg_var.seed) + lcg_var.increment) mod lcg_var.modulo;
  exit(lcg_var.seed);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  lcg_var := make_lcg(1664525, 1013904223, 4294967296, _now());
  i := 0;
  while i < 5 do begin
  writeln(IntToStr(next_number(lcg_var)));
  i := i + 1;
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
