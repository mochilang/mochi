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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function bernoulli(n: integer): BigRat; forward;
function binom(n: integer; k: integer): int64; forward;
function coeff(p: integer; j: integer): BigRat; forward;
procedure main(); forward;
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
function binom(n: integer; k: integer): int64;
var
  binom_kk: integer;
  binom_res: int64;
  binom_i: integer;
begin
  if (k < 0) or (k > n) then begin
  exit(0);
end;
  binom_kk := k;
  if binom_kk > (n - binom_kk) then begin
  binom_kk := n - binom_kk;
end;
  binom_res := 1;
  binom_i := 0;
  while binom_i < binom_kk do begin
  binom_res := binom_res * (n - binom_i);
  binom_i := binom_i + 1;
  binom_res := binom_res div binom_i;
end;
  exit(binom_res);
end;
function coeff(p: integer; j: integer): BigRat;
var
  coeff_base: BigRat;
  coeff_c: BigRat;
begin
  coeff_base := _div(_bigrat(1), _bigrat(p + 1));
  coeff_c := coeff_base;
  if (j mod 2) = 1 then begin
  coeff_c := _sub(_bigrat(0), coeff_c);
end;
  coeff_c := _mul(coeff_c, _bigrat(binom(p + 1, j)));
  coeff_c := _mul(coeff_c, bernoulli(j));
  exit(coeff_c);
end;
procedure main();
var
  main_p: integer;
  main_line: string;
  main_j: integer;
  main_c: BigRat;
  main_exp: integer;
begin
  main_p := 0;
  while main_p < 10 do begin
  main_line := IntToStr(main_p) + ' :';
  main_j := 0;
  while main_j <= main_p do begin
  main_c := coeff(main_p, main_j);
  if ((IntToStr(num(main_c)) + '/') + IntToStr(denom(main_c))) <> '0/1' then begin
  main_line := ((main_line + ' ') + ((IntToStr(num(main_c)) + '/') + IntToStr(denom(main_c)))) + '×n';
  main_exp := (main_p + 1) - main_j;
  if main_exp > 1 then begin
  main_line := (main_line + '^') + IntToStr(main_exp);
end;
end;
  main_j := main_j + 1;
end;
  writeln(main_line);
  main_p := main_p + 1;
end;
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
