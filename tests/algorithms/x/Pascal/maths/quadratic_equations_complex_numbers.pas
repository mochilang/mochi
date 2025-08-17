{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Complex = record
  re: real;
  im: real;
end;
type ComplexArray = array of Complex;
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
  a: real;
  b: real;
  c: real;
  d: real;
  r: Complex;
  x: real;
function makeComplex(re: real; im: real): Complex; forward;
function add(a: Complex; b: Complex): Complex; forward;
function sub(a: Complex; b: Complex): Complex; forward;
function div_real(a: Complex; r: real): Complex; forward;
function sqrt_newton(x: real): real; forward;
function sqrt_to_complex(d: real): Complex; forward;
function quadratic_roots(a: real; b: real; c: real): ComplexArray; forward;
function root_str(r: Complex): string; forward;
procedure main(); forward;
function makeComplex(re: real; im: real): Complex;
begin
  Result.re := re;
  Result.im := im;
end;
function add(a: Complex; b: Complex): Complex;
begin
  exit(makeComplex(a.re + b.re, a.im + b.im));
end;
function sub(a: Complex; b: Complex): Complex;
begin
  exit(makeComplex(a.re - b.re, a.im - b.im));
end;
function div_real(a: Complex; r: real): Complex;
begin
  exit(makeComplex(a.re / r, a.im / r));
end;
function sqrt_newton(x: real): real;
var
  sqrt_newton_guess: real;
  sqrt_newton_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrt_newton_guess := x / 2;
  sqrt_newton_i := 0;
  while sqrt_newton_i < 20 do begin
  sqrt_newton_guess := (sqrt_newton_guess + (x / sqrt_newton_guess)) / 2;
  sqrt_newton_i := sqrt_newton_i + 1;
end;
  exit(sqrt_newton_guess);
end;
function sqrt_to_complex(d: real): Complex;
begin
  if d >= 0 then begin
  exit(makeComplex(sqrt_newton(d), 0));
end;
  exit(makeComplex(0, sqrt_newton(-d)));
end;
function quadratic_roots(a: real; b: real; c: real): ComplexArray;
var
  quadratic_roots_delta: real;
  quadratic_roots_sqrt_d: Complex;
  quadratic_roots_minus_b: Complex;
  quadratic_roots_two_a: real;
  quadratic_roots_root1: Complex;
  quadratic_roots_root2: Complex;
begin
  if a = 0 then begin
  writeln('ValueError: coefficient ''a'' must not be zero');
  exit([]);
end;
  quadratic_roots_delta := (b * b) - ((4 * a) * c);
  quadratic_roots_sqrt_d := sqrt_to_complex(quadratic_roots_delta);
  quadratic_roots_minus_b := makeComplex(-b, 0);
  quadratic_roots_two_a := 2 * a;
  quadratic_roots_root1 := div_real(add(quadratic_roots_minus_b, quadratic_roots_sqrt_d), quadratic_roots_two_a);
  quadratic_roots_root2 := div_real(sub(quadratic_roots_minus_b, quadratic_roots_sqrt_d), quadratic_roots_two_a);
  exit([quadratic_roots_root1, quadratic_roots_root2]);
end;
function root_str(r: Complex): string;
var
  root_str_s: string;
begin
  if r.im = 0 then begin
  exit(FloatToStr(r.re));
end;
  root_str_s := FloatToStr(r.re);
  if r.im >= 0 then begin
  root_str_s := ((root_str_s + '+') + FloatToStr(r.im)) + 'i';
end else begin
  root_str_s := (root_str_s + FloatToStr(r.im)) + 'i';
end;
  exit(root_str_s);
end;
procedure main();
var
  main_roots: ComplexArray;
begin
  main_roots := quadratic_roots(5, 6, 1);
  if Length(main_roots) = 2 then begin
  writeln((('The solutions are: ' + root_str(main_roots[0])) + ' and ') + root_str(main_roots[1]));
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
