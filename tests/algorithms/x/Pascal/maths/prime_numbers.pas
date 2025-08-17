{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  max_n: integer;
function slow_primes(max_n: integer): IntArray; forward;
function primes(max_n: integer): IntArray; forward;
function fast_primes(max_n: integer): IntArray; forward;
function slow_primes(max_n: integer): IntArray;
var
  slow_primes_result_: array of integer;
  slow_primes_i: integer;
  slow_primes_j: integer;
  slow_primes_is_prime: boolean;
begin
  slow_primes_result_ := [];
  slow_primes_i := 2;
  while slow_primes_i <= max_n do begin
  slow_primes_j := 2;
  slow_primes_is_prime := true;
  while slow_primes_j < slow_primes_i do begin
  if (slow_primes_i mod slow_primes_j) = 0 then begin
  slow_primes_is_prime := false;
  break;
end;
  slow_primes_j := slow_primes_j + 1;
end;
  if slow_primes_is_prime then begin
  slow_primes_result_ := concat(slow_primes_result_, IntArray([slow_primes_i]));
end;
  slow_primes_i := slow_primes_i + 1;
end;
  exit(slow_primes_result_);
end;
function primes(max_n: integer): IntArray;
var
  primes_result_: array of integer;
  primes_i: integer;
  primes_j: integer;
  primes_is_prime: boolean;
begin
  primes_result_ := [];
  primes_i := 2;
  while primes_i <= max_n do begin
  primes_j := 2;
  primes_is_prime := true;
  while (primes_j * primes_j) <= primes_i do begin
  if (primes_i mod primes_j) = 0 then begin
  primes_is_prime := false;
  break;
end;
  primes_j := primes_j + 1;
end;
  if primes_is_prime then begin
  primes_result_ := concat(primes_result_, IntArray([primes_i]));
end;
  primes_i := primes_i + 1;
end;
  exit(primes_result_);
end;
function fast_primes(max_n: integer): IntArray;
var
  fast_primes_result_: array of integer;
  fast_primes_i: integer;
  fast_primes_j: integer;
  fast_primes_is_prime: boolean;
begin
  fast_primes_result_ := [];
  if max_n >= 2 then begin
  fast_primes_result_ := concat(fast_primes_result_, IntArray([2]));
end;
  fast_primes_i := 3;
  while fast_primes_i <= max_n do begin
  fast_primes_j := 3;
  fast_primes_is_prime := true;
  while (fast_primes_j * fast_primes_j) <= fast_primes_i do begin
  if (fast_primes_i mod fast_primes_j) = 0 then begin
  fast_primes_is_prime := false;
  break;
end;
  fast_primes_j := fast_primes_j + 2;
end;
  if fast_primes_is_prime then begin
  fast_primes_result_ := concat(fast_primes_result_, IntArray([fast_primes_i]));
end;
  fast_primes_i := fast_primes_i + 2;
end;
  exit(fast_primes_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_int_to_str(slow_primes(25)));
  writeln(list_int_to_str(primes(25)));
  writeln(list_int_to_str(fast_primes(25)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
