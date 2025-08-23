{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type BoolArray = array of boolean;
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
function isqrt(isqrt_n: int64): int64; forward;
function solution(solution_n: int64): int64; forward;
function isqrt(isqrt_n: int64): int64;
var
  isqrt_r: int64;
begin
  isqrt_r := 0;
  while ((isqrt_r + 1) * (isqrt_r + 1)) <= isqrt_n do begin
  isqrt_r := isqrt_r + 1;
end;
  exit(isqrt_r);
end;
function solution(solution_n: int64): int64;
var
  solution_sieve: array of boolean;
  solution_i: int64;
  solution_limit: int64;
  solution_p: int64;
  solution_j: int64;
  solution_sum: int64;
  solution_k: int64;
begin
  solution_sieve := [];
  solution_i := 0;
  while solution_i <= solution_n do begin
  solution_sieve := concat(solution_sieve, [false]);
  solution_i := solution_i + 1;
end;
  solution_sieve[0] := true;
  solution_sieve[1] := true;
  solution_limit := isqrt(solution_n);
  solution_p := 2;
  while solution_p <= solution_limit do begin
  if not solution_sieve[solution_p] then begin
  solution_j := solution_p * solution_p;
  while solution_j <= solution_n do begin
  solution_sieve[solution_j] := true;
  solution_j := solution_j + solution_p;
end;
end;
  solution_p := solution_p + 1;
end;
  solution_sum := 0;
  solution_k := 2;
  while solution_k < solution_n do begin
  if not solution_sieve[solution_k] then begin
  solution_sum := solution_sum + solution_k;
end;
  solution_k := solution_k + 1;
end;
  exit(solution_sum);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(solution(20000)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
