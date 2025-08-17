{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type Complex = record
  re: real;
  im: real;
end;
type RealArray = array of real;
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
  PI: real;
  A_43: array of real;
  B_44: array of real;
  product: RealArray;
  a: RealArray;
  b: RealArray;
  invert: boolean;
  l: RealArray;
  n: integer;
  ndigits: integer;
  s: real;
  theta: real;
  value: Complex;
  x: real;
function makeComplex(re: real; im: real): Complex; forward;
function c_add(a: Complex; b: Complex): Complex; forward;
function c_sub(a: Complex; b: Complex): Complex; forward;
function c_mul(a: Complex; b: Complex): Complex; forward;
function c_mul_scalar(a: Complex; s: real): Complex; forward;
function c_div_scalar(a: Complex; s: real): Complex; forward;
function sin_taylor(x: real): real; forward;
function cos_taylor(x: real): real; forward;
function exp_i(theta: real): Complex; forward;
function make_complex_list(n: integer; value: Complex): ComplexArray; forward;
function fft(a: ComplexArray; invert: boolean): ComplexArray; forward;
function floor(x: real): real; forward;
function pow10(n: integer): real; forward;
function round_to(x: real; ndigits: integer): real; forward;
function list_to_string(l: RealArray): string; forward;
function multiply_poly(a: RealArray; b: RealArray): RealArray; forward;
function makeComplex(re: real; im: real): Complex;
begin
  Result.re := re;
  Result.im := im;
end;
function c_add(a: Complex; b: Complex): Complex;
begin
  exit(makeComplex(a.re + b.re, a.im + b.im));
end;
function c_sub(a: Complex; b: Complex): Complex;
begin
  exit(makeComplex(a.re - b.re, a.im - b.im));
end;
function c_mul(a: Complex; b: Complex): Complex;
begin
  exit(makeComplex((a.re * b.re) - (a.im * b.im), (a.re * b.im) + (a.im * b.re)));
end;
function c_mul_scalar(a: Complex; s: real): Complex;
begin
  exit(makeComplex(a.re * s, a.im * s));
end;
function c_div_scalar(a: Complex; s: real): Complex;
begin
  exit(makeComplex(a.re / s, a.im / s));
end;
function sin_taylor(x: real): real;
var
  sin_taylor_term: real;
  sin_taylor_sum: real;
  sin_taylor_i: integer;
  sin_taylor_k1: real;
  sin_taylor_k2: real;
begin
  sin_taylor_term := x;
  sin_taylor_sum := x;
  sin_taylor_i := 1;
  while sin_taylor_i < 10 do begin
  sin_taylor_k1 := 2 * Double(sin_taylor_i);
  sin_taylor_k2 := sin_taylor_k1 + 1;
  sin_taylor_term := ((-sin_taylor_term * x) * x) / (sin_taylor_k1 * sin_taylor_k2);
  sin_taylor_sum := sin_taylor_sum + sin_taylor_term;
  sin_taylor_i := sin_taylor_i + 1;
end;
  exit(sin_taylor_sum);
end;
function cos_taylor(x: real): real;
var
  cos_taylor_term: real;
  cos_taylor_sum: real;
  cos_taylor_i: integer;
  cos_taylor_k1: real;
  cos_taylor_k2: real;
begin
  cos_taylor_term := 1;
  cos_taylor_sum := 1;
  cos_taylor_i := 1;
  while cos_taylor_i < 10 do begin
  cos_taylor_k1 := (2 * Double(cos_taylor_i)) - 1;
  cos_taylor_k2 := 2 * Double(cos_taylor_i);
  cos_taylor_term := ((-cos_taylor_term * x) * x) / (cos_taylor_k1 * cos_taylor_k2);
  cos_taylor_sum := cos_taylor_sum + cos_taylor_term;
  cos_taylor_i := cos_taylor_i + 1;
end;
  exit(cos_taylor_sum);
end;
function exp_i(theta: real): Complex;
begin
  exit(makeComplex(cos_taylor(theta), sin_taylor(theta)));
end;
function make_complex_list(n: integer; value: Complex): ComplexArray;
var
  make_complex_list_arr: array of Complex;
  make_complex_list_i: integer;
begin
  make_complex_list_arr := [];
  make_complex_list_i := 0;
  while make_complex_list_i < n do begin
  make_complex_list_arr := concat(make_complex_list_arr, [value]);
  make_complex_list_i := make_complex_list_i + 1;
end;
  exit(make_complex_list_arr);
end;
function fft(a: ComplexArray; invert: boolean): ComplexArray;
var
  fft_n: integer;
  fft_a0: array of Complex;
  fft_a1: array of Complex;
  fft_i: integer;
  fft_y0: array of Complex;
  fft_y1: array of Complex;
  fft_angle: real;
  fft_w: Complex;
  fft_wn: Complex;
  fft_y: array of Complex;
  fft_t: Complex;
  fft_u: Complex;
  fft_even: Complex;
  fft_odd: Complex;
begin
  fft_n := Length(a);
  if fft_n = 1 then begin
  exit([a[0]]);
end;
  fft_a0 := [];
  fft_a1 := [];
  fft_i := 0;
  while fft_i < (fft_n div 2) do begin
  fft_a0 := concat(fft_a0, [a[2 * fft_i]]);
  fft_a1 := concat(fft_a1, [a[(2 * fft_i) + 1]]);
  fft_i := fft_i + 1;
end;
  fft_y0 := fft(fft_a0, invert);
  fft_y1 := fft(fft_a1, invert);
  fft_angle := ((2 * PI) / Double(fft_n)) * IfThen(invert, -1, 1);
  fft_w := makeComplex(1, 0);
  fft_wn := exp_i(fft_angle);
  fft_y := make_complex_list(fft_n, makeComplex(0, 0));
  fft_i := 0;
  while fft_i < (fft_n div 2) do begin
  fft_t := c_mul(fft_w, fft_y1[fft_i]);
  fft_u := fft_y0[fft_i];
  fft_even := c_add(fft_u, fft_t);
  fft_odd := c_sub(fft_u, fft_t);
  if invert then begin
  fft_even := c_div_scalar(fft_even, 2);
  fft_odd := c_div_scalar(fft_odd, 2);
end;
  fft_y[fft_i] := fft_even;
  fft_y[fft_i + (fft_n div 2)] := fft_odd;
  fft_w := c_mul(fft_w, fft_wn);
  fft_i := fft_i + 1;
end;
  exit(fft_y);
end;
function floor(x: real): real;
var
  floor_i: integer;
begin
  floor_i := Trunc(x);
  if Double(floor_i) > x then begin
  floor_i := floor_i - 1;
end;
  exit(Double(floor_i));
end;
function pow10(n: integer): real;
var
  pow10_p: real;
  pow10_i: integer;
begin
  pow10_p := 1;
  pow10_i := 0;
  while pow10_i < n do begin
  pow10_p := pow10_p * 10;
  pow10_i := pow10_i + 1;
end;
  exit(pow10_p);
end;
function round_to(x: real; ndigits: integer): real;
var
  round_to_m: real;
begin
  round_to_m := pow10(ndigits);
  exit(Floor((x * round_to_m) + 0.5) / round_to_m);
end;
function list_to_string(l: RealArray): string;
var
  list_to_string_s: string;
  list_to_string_i: integer;
begin
  list_to_string_s := '[';
  list_to_string_i := 0;
  while list_to_string_i < Length(l) do begin
  list_to_string_s := list_to_string_s + FloatToStr(l[list_to_string_i]);
  if (list_to_string_i + 1) < Length(l) then begin
  list_to_string_s := list_to_string_s + ', ';
end;
  list_to_string_i := list_to_string_i + 1;
end;
  list_to_string_s := list_to_string_s + ']';
  exit(list_to_string_s);
end;
function multiply_poly(a: RealArray; b: RealArray): RealArray;
var
  multiply_poly_n: integer;
  multiply_poly_fa: ComplexArray;
  multiply_poly_fb: ComplexArray;
  multiply_poly_i: integer;
  multiply_poly_res: array of real;
  multiply_poly_val: Complex;
begin
  multiply_poly_n := 1;
  while multiply_poly_n < ((Length(a) + Length(b)) - 1) do begin
  multiply_poly_n := multiply_poly_n * 2;
end;
  multiply_poly_fa := make_complex_list(multiply_poly_n, makeComplex(0, 0));
  multiply_poly_fb := make_complex_list(multiply_poly_n, makeComplex(0, 0));
  multiply_poly_i := 0;
  while multiply_poly_i < Length(a) do begin
  multiply_poly_fa[multiply_poly_i] := makeComplex(a[multiply_poly_i], 0);
  multiply_poly_i := multiply_poly_i + 1;
end;
  multiply_poly_i := 0;
  while multiply_poly_i < Length(b) do begin
  multiply_poly_fb[multiply_poly_i] := makeComplex(b[multiply_poly_i], 0);
  multiply_poly_i := multiply_poly_i + 1;
end;
  multiply_poly_fa := fft(multiply_poly_fa, false);
  multiply_poly_fb := fft(multiply_poly_fb, false);
  multiply_poly_i := 0;
  while multiply_poly_i < multiply_poly_n do begin
  multiply_poly_fa[multiply_poly_i] := c_mul(multiply_poly_fa[multiply_poly_i], multiply_poly_fb[multiply_poly_i]);
  multiply_poly_i := multiply_poly_i + 1;
end;
  multiply_poly_fa := fft(multiply_poly_fa, true);
  multiply_poly_res := [];
  multiply_poly_i := 0;
  while multiply_poly_i < ((Length(a) + Length(b)) - 1) do begin
  multiply_poly_val := multiply_poly_fa[multiply_poly_i];
  multiply_poly_res := concat(multiply_poly_res, [round_to(multiply_poly_val.re, 8)]);
  multiply_poly_i := multiply_poly_i + 1;
end;
  while (Length(multiply_poly_res) > 0) and (multiply_poly_res[Length(multiply_poly_res) - 1] = 0) do begin
  multiply_poly_res := copy(multiply_poly_res, 0, (Length(multiply_poly_res) - 1 - (0)));
end;
  exit(multiply_poly_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  A_43 := [0, 1, 0, 2];
  B_44 := [2, 3, 4, 0];
  product := multiply_poly(A_43, B_44);
  writeln(list_to_string(product));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
