{$mode objfpc}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function abs(x: real): real; forward;
function sqrtApprox(x: real): real; forward;
function agmPi(): real; forward;
procedure main(); forward;
function abs(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  sqrtApprox_guess := x;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function agmPi(): real;
var
  agmPi_a: real;
  agmPi_g: real;
  agmPi_sum: real;
  agmPi_pow: real;
  agmPi_t: real;
  agmPi_u: real;
  agmPi_diff: real;
  agmPi_pi: real;
begin
  agmPi_a := 1;
  agmPi_g := 1 / sqrtApprox(2);
  agmPi_sum := 0;
  agmPi_pow := 2;
  while abs(agmPi_a - agmPi_g) > 1e-15 do begin
  agmPi_t := (agmPi_a + agmPi_g) / 2;
  agmPi_u := sqrtApprox(agmPi_a * agmPi_g);
  agmPi_a := agmPi_t;
  agmPi_g := agmPi_u;
  agmPi_pow := agmPi_pow * 2;
  agmPi_diff := (agmPi_a * agmPi_a) - (agmPi_g * agmPi_g);
  agmPi_sum := agmPi_sum + (agmPi_diff * agmPi_pow);
end;
  agmPi_pi := ((4 * agmPi_a) * agmPi_a) / (1 - agmPi_sum);
  exit(agmPi_pi);
end;
procedure main();
begin
  writeln(FloatToStr(agmPi()));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
