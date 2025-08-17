{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math, fgl;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  factors: IntArray;
  n: integer;
function primeFactors(n: integer): IntArray; forward;
function isSquareFree(factors: IntArray): boolean; forward;
function mobius(n: integer): integer; forward;
function primeFactors(n: integer): IntArray;
var
  primeFactors_i: integer;
  primeFactors_factors: array of integer;
begin
  primeFactors_i := 2;
  primeFactors_factors := [];
  while (primeFactors_i * primeFactors_i) <= n do begin
  if (n mod primeFactors_i) = 0 then begin
  primeFactors_factors := concat(primeFactors_factors, IntArray([primeFactors_i]));
  n := n div primeFactors_i;
end else begin
  primeFactors_i := primeFactors_i + 1;
end;
end;
  if n > 1 then begin
  primeFactors_factors := concat(primeFactors_factors, IntArray([n]));
end;
  exit(primeFactors_factors);
end;
function isSquareFree(factors: IntArray): boolean;
var
  isSquareFree_seen: specialize TFPGMap<integer, boolean>;
  isSquareFree_f: integer;
begin
  isSquareFree_seen := specialize TFPGMap<integer, boolean>.Create();
  for isSquareFree_f in factors do begin
  if isSquareFree_seen.IndexOf(isSquareFree_f) <> -1 then begin
  exit(false);
end;
  isSquareFree_seen[isSquareFree_f] := true;
end;
  exit(true);
end;
function mobius(n: integer): integer;
var
  mobius_factors: IntArray;
begin
  mobius_factors := primeFactors(n);
  if isSquareFree(mobius_factors) then begin
  exit(IfThen((Length(mobius_factors) mod 2) = 0, 1, -1));
end;
  exit(0);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(mobius(24));
  writeln(mobius(-1));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
