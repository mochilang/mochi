{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type LCG = record
  multiplier: int64;
  increment: int64;
  modulo: int64;
  seed: int64;
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
function _to_float(x: int64): real;
begin
  _to_float := x;
end;
function to_float(x: int64): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  lcg_4_var: LCG;
  i: int64;
function makeLCG(multiplier: int64; increment: int64; modulo: int64; seed: int64): LCG; forward;
function make_lcg(make_lcg_multiplier: int64; make_lcg_increment: int64; make_lcg_modulo: int64; make_lcg_seed: int64): LCG; forward;
function next_number(next_number_lcg_var: LCG): int64; forward;
function makeLCG(multiplier: int64; increment: int64; modulo: int64; seed: int64): LCG;
begin
  Result.multiplier := multiplier;
  Result.increment := increment;
  Result.modulo := modulo;
  Result.seed := seed;
end;
function make_lcg(make_lcg_multiplier: int64; make_lcg_increment: int64; make_lcg_modulo: int64; make_lcg_seed: int64): LCG;
begin
  exit(makeLCG(make_lcg_multiplier, make_lcg_increment, make_lcg_modulo, make_lcg_seed));
end;
function next_number(next_number_lcg_var: LCG): int64;
begin
  next_number_lcg_var.seed := ((next_number_lcg_var.multiplier * next_number_lcg_var.seed) + next_number_lcg_var.increment) mod next_number_lcg_var.modulo;
  exit(next_number_lcg_var.seed);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  lcg_4_var := make_lcg(1664525, 1013904223, 4294967296, _now());
  i := 0;
  while i < 5 do begin
  writeln(IntToStr(next_number(lcg_4_var)));
  i := i + 1;
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
