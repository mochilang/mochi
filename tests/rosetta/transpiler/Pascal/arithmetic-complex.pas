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
type Complex = record
  re: real;
  im: real;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  a: Complex;
  b: Complex;
function makeComplex(re: real; im: real): Complex; forward;
function add(a: Complex; b: Complex): Complex; forward;
function mul(a: Complex; b: Complex): Complex; forward;
function neg(a: Complex): Complex; forward;
function inv(a: Complex): Complex; forward;
function conj(a: Complex): Complex; forward;
function cstr(a: Complex): string; forward;
function makeComplex(re: real; im: real): Complex;
begin
  Result.re := re;
  Result.im := im;
end;
function add(a: Complex; b: Complex): Complex;
begin
  exit(makeComplex(a.re + b.re, a.im + b.im));
end;
function mul(a: Complex; b: Complex): Complex;
begin
  exit(makeComplex((a.re * b.re) - (a.im * b.im), (a.re * b.im) + (a.im * b.re)));
end;
function neg(a: Complex): Complex;
begin
  exit(makeComplex(-a.re, -a.im));
end;
function inv(a: Complex): Complex;
var
  inv_denom: real;
begin
  inv_denom := (a.re * a.re) + (a.im * a.im);
  exit(makeComplex(a.re / inv_denom, -a.im / inv_denom));
end;
function conj(a: Complex): Complex;
begin
  exit(makeComplex(a.re, -a.im));
end;
function cstr(a: Complex): string;
var
  cstr_s: string;
begin
  cstr_s := '(' + FloatToStr(a.re);
  if a.im >= 0 then begin
  cstr_s := ((cstr_s + '+') + FloatToStr(a.im)) + 'i)';
end else begin
  cstr_s := (cstr_s + FloatToStr(a.im)) + 'i)';
end;
  exit(cstr_s);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  a := makeComplex(1, 1);
  b := makeComplex(3.14159, 1.25);
  writeln('a:       ' + cstr(a));
  writeln('b:       ' + cstr(b));
  writeln('a + b:   ' + cstr(add(a, b)));
  writeln('a * b:   ' + cstr(mul(a, b)));
  writeln('-a:      ' + cstr(neg(a)));
  writeln('1 / a:   ' + cstr(inv(a)));
  writeln('a̅:       ' + cstr(conj(a)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
