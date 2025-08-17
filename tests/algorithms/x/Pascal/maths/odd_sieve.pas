{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type BoolArray = array of boolean;
type IntArray = array of integer;
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
procedure show_list(xs: array of integer);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  num: integer;
function odd_sieve(num: integer): IntArray; forward;
function odd_sieve(num: integer): IntArray;
var
  odd_sieve_size: integer;
  odd_sieve_sieve: array of boolean;
  odd_sieve_idx: integer;
  odd_sieve_i: integer;
  odd_sieve_s_idx: integer;
  odd_sieve_j: integer;
  odd_sieve_j_idx: integer;
  odd_sieve_primes: array of integer;
  odd_sieve_n: integer;
  odd_sieve_k: integer;
begin
  if num <= 2 then begin
  exit([]);
end;
  if num = 3 then begin
  exit([2]);
end;
  odd_sieve_size := (num div 2) - 1;
  odd_sieve_sieve := [];
  odd_sieve_idx := 0;
  while odd_sieve_idx < odd_sieve_size do begin
  odd_sieve_sieve := concat(odd_sieve_sieve, [true]);
  odd_sieve_idx := odd_sieve_idx + 1;
end;
  odd_sieve_i := 3;
  while (odd_sieve_i * odd_sieve_i) <= num do begin
  odd_sieve_s_idx := (odd_sieve_i div 2) - 1;
  if odd_sieve_sieve[odd_sieve_s_idx] then begin
  odd_sieve_j := odd_sieve_i * odd_sieve_i;
  while odd_sieve_j < num do begin
  odd_sieve_j_idx := (odd_sieve_j div 2) - 1;
  odd_sieve_sieve[odd_sieve_j_idx] := false;
  odd_sieve_j := odd_sieve_j + (2 * odd_sieve_i);
end;
end;
  odd_sieve_i := odd_sieve_i + 2;
end;
  odd_sieve_primes := [2];
  odd_sieve_n := 3;
  odd_sieve_k := 0;
  while odd_sieve_n < num do begin
  if odd_sieve_sieve[odd_sieve_k] then begin
  odd_sieve_primes := concat(odd_sieve_primes, IntArray([odd_sieve_n]));
end;
  odd_sieve_n := odd_sieve_n + 2;
  odd_sieve_k := odd_sieve_k + 1;
end;
  exit(odd_sieve_primes);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  show_list(odd_sieve(2));
  show_list(odd_sieve(3));
  show_list(odd_sieve(10));
  show_list(odd_sieve(20));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
