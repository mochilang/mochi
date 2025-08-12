{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  n: integer;
function prime_factors(n: integer): IntArray; forward;
function liouville_lambda(n: integer): integer; forward;
function prime_factors(n: integer): IntArray;
var
  prime_factors_i: integer;
  prime_factors_x: integer;
  prime_factors_factors: array of integer;
begin
  prime_factors_i := 2;
  prime_factors_x := n;
  prime_factors_factors := [];
  while (prime_factors_i * prime_factors_i) <= prime_factors_x do begin
  if (prime_factors_x mod prime_factors_i) = 0 then begin
  prime_factors_factors := concat(prime_factors_factors, IntArray([prime_factors_i]));
  prime_factors_x := Trunc(prime_factors_x div prime_factors_i);
end else begin
  prime_factors_i := prime_factors_i + 1;
end;
end;
  if prime_factors_x > 1 then begin
  prime_factors_factors := concat(prime_factors_factors, IntArray([prime_factors_x]));
end;
  exit(prime_factors_factors);
end;
function liouville_lambda(n: integer): integer;
var
  liouville_lambda_cnt: integer;
begin
  if n < 1 then begin
  panic('Input must be a positive integer');
end;
  liouville_lambda_cnt := Length(prime_factors(n));
  if (liouville_lambda_cnt mod 2) = 0 then begin
  exit(1);
end;
  exit(0 - 1);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(liouville_lambda(10));
  writeln(liouville_lambda(11));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
