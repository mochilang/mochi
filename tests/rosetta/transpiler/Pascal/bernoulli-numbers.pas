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
type BigRat = record num: int64; den: int64; end;
function _gcd(a, b: int64): int64;
var t: int64;
begin
  while b <> 0 do begin
    t := b;
    b := a mod b;
    a := t;
  end;
  if a < 0 then a := -a;
  _gcd := a;
end;
function _bigrat(n: int64): BigRat;
begin
  _bigrat.num := n; _bigrat.den := 1;
end;
function _bigrat2(n, d: int64): BigRat;
var g: int64;
begin
  if d < 0 then begin n := -n; d := -d; end;
  g := _gcd(n, d);
  _bigrat2.num := n div g;
  _bigrat2.den := d div g;
end;
function _add(a, b: BigRat): BigRat;
begin
  _add := _bigrat2(a.num*b.den + b.num*a.den, a.den*b.den);
end;
function _sub(a, b: BigRat): BigRat;
begin
  _sub := _bigrat2(a.num*b.den - b.num*a.den, a.den*b.den);
end;
function _mul(a, b: BigRat): BigRat;
begin
  _mul := _bigrat2(a.num*b.num, a.den*b.den);
end;
function _div(a, b: BigRat): BigRat;
begin
  _div := _bigrat2(a.num*b.den, a.den*b.num);
end;
function num(r: BigRat): int64; begin num := r.num; end;
function denom(r: BigRat): int64; begin denom := r.den; end;
function padStart(s: string; l: integer; c: char): string;
var d: integer;
begin
  d := l - Length(s);
  if d > 0 then padStart := StringOfChar(c, d) + s else padStart := s;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  i: integer;
  b: BigRat;
  numStr: string;
  denStr: string;
function bernoulli(n: integer): BigRat; forward;
function padStart(s: string; width: integer; pad: string): string; forward;
function bernoulli(n: integer): BigRat;
var
  bernoulli_a: array of BigRat;
  bernoulli_m: integer;
  bernoulli_j: integer;
begin
  bernoulli_a := [];
  bernoulli_m := 0;
  while bernoulli_m <= n do begin
  bernoulli_a := concat(bernoulli_a, [_div(_bigrat(1), _bigrat(bernoulli_m + 1))]);
  bernoulli_j := bernoulli_m;
  while bernoulli_j >= 1 do begin
  bernoulli_a[bernoulli_j - 1] := _mul(_bigrat(bernoulli_j), _sub(bernoulli_a[bernoulli_j - 1], bernoulli_a[bernoulli_j]));
  bernoulli_j := bernoulli_j - 1;
end;
  bernoulli_m := bernoulli_m + 1;
end;
  exit(bernoulli_a[0]);
end;
function padStart(s: string; width: integer; pad: string): string;
var
  padStart_out: string;
begin
  padStart_out := s;
  while Length(padStart_out) < width do begin
  padStart_out := pad + padStart_out;
end;
  exit(padStart_out);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  for i := 0 to (61 - 1) do begin
  b := bernoulli(i);
  if num(b) <> 0 then begin
  numStr := IntToStr(num(b));
  denStr := IntToStr(denom(b));
  writeln((((('B(' + padStart(IntToStr(i), 2, ' ')) + ') =') + padStart(numStr, 45, ' ')) + '/') + denStr);
end;
end;
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
