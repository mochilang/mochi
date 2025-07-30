{$mode objfpc}
program Main;
uses SysUtils;
type BigRatArray = array of BigRat;
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
function faulhaberRow(p: integer): BigRatArray; forward;
function ratStr(r: BigRat): string; forward;
function endsWith(s: string; suf: string): boolean; forward;
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
  if n <> 1 then begin
  exit(bernoulli_a[0]);
end;
  exit(_sub(_bigrat(0), bernoulli_a[0]));
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
function faulhaberRow(p: integer): BigRatArray;
var
  faulhaberRow_coeffs: array of BigRat;
  faulhaberRow_i: integer;
  faulhaberRow_j: integer;
  faulhaberRow_sign: integer;
  faulhaberRow_c: BigRat;
begin
  faulhaberRow_coeffs := [];
  faulhaberRow_i := 0;
  while faulhaberRow_i <= p do begin
  faulhaberRow_coeffs := concat(faulhaberRow_coeffs, [_bigrat(0)]);
  faulhaberRow_i := faulhaberRow_i + 1;
end;
  faulhaberRow_j := 0;
  faulhaberRow_sign := -1;
  while faulhaberRow_j <= p do begin
  faulhaberRow_sign := -faulhaberRow_sign;
  faulhaberRow_c := _div(_bigrat(1), _bigrat(p + 1));
  if faulhaberRow_sign < 0 then begin
  faulhaberRow_c := _sub(_bigrat(0), faulhaberRow_c);
end;
  faulhaberRow_c := _mul(faulhaberRow_c, _bigrat(binom(p + 1, faulhaberRow_j)));
  faulhaberRow_c := _mul(faulhaberRow_c, bernoulli(faulhaberRow_j));
  faulhaberRow_coeffs[p - faulhaberRow_j] := faulhaberRow_c;
  faulhaberRow_j := faulhaberRow_j + 1;
end;
  exit(faulhaberRow_coeffs);
end;
function ratStr(r: BigRat): string;
var
  ratStr_s: string;
begin
  ratStr_s := (IntToStr(num(r)) + '/') + IntToStr(denom(r));
  if endsWith(ratStr_s, '/1') then begin
  exit(substr(ratStr_s, 0, Length(ratStr_s) - 2));
end;
  exit(ratStr_s);
end;
function endsWith(s: string; suf: string): boolean;
begin
  if Length(s) < Length(suf) then begin
  exit(false);
end;
  exit(copy(s, Length(s) - Length(suf)+1, (Length(s) - (Length(s) - Length(suf)))) = suf);
end;
procedure main();
var
  main_p: integer;
  main_row: BigRatArray;
  main_line: string;
  main_idx: integer;
  main_k: integer;
  main_coeffs: BigRatArray;
  main_nn: BigRat;
  main_np: BigRat;
  main_sum: BigRat;
  main_i: integer;
begin
  main_p := 0;
  while main_p < 10 do begin
  main_row := faulhaberRow(main_p);
  main_line := '';
  main_idx := 0;
  while main_idx < Length(main_row) do begin
  main_line := main_line + padStart(ratStr(main_row[main_idx]), 5, ' ');
  if main_idx < (Length(main_row) - 1) then begin
  main_line := main_line + '  ';
end;
  main_idx := main_idx + 1;
end;
  writeln(main_line);
  main_p := main_p + 1;
end;
  writeln('');
  main_k := 17;
  main_coeffs := faulhaberRow(main_k);
  main_nn := _bigrat(1000);
  main_np := _bigrat(1);
  main_sum := _bigrat(0);
  main_i := 0;
  while main_i < Length(main_coeffs) do begin
  main_np := _mul(main_np, main_nn);
  main_sum := _add(main_sum, _mul(main_coeffs[main_i], main_np));
  main_i := main_i + 1;
end;
  writeln(ratStr(main_sum));
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
