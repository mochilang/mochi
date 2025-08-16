{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
type RealArrayArray = array of RealArray;
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
  sample_rate: integer;
  size: integer;
  audio: array of real;
  n: integer;
  t: real;
  coeffs: RealArray;
  c: real;
  frame: RealArray;
  mat: RealArrayArray;
  bins: integer;
  x: real;
  dct_filter_num: integer;
  dct_num: integer;
  filter_num: integer;
  vec: RealArray;
  spectrum_size: integer;
function sinApprox(x: real): real; forward;
function cosApprox(x: real): real; forward;
function expApprox(x: real): real; forward;
function ln_(x: real): real; forward;
function log10(x: real): real; forward;
function sqrtApprox(x: real): real; forward;
function absf(x: real): real; forward;
function normalize(audio: RealArray): RealArray; forward;
function dft(frame: RealArray; bins: integer): RealArray; forward;
function triangular_filters(bins: integer; spectrum_size: integer): RealArrayArray; forward;
function dot(mat: RealArrayArray; vec: RealArray): RealArray; forward;
function discrete_cosine_transform(dct_filter_num: integer; filter_num: integer): RealArrayArray; forward;
function mfcc(audio: RealArray; bins: integer; dct_num: integer): RealArray; forward;
function sinApprox(x: real): real;
var
  sinApprox_term: real;
  sinApprox_sum: real;
  sinApprox_n: integer;
  sinApprox_denom: real;
begin
  sinApprox_term := x;
  sinApprox_sum := x;
  sinApprox_n := 1;
  while sinApprox_n <= 10 do begin
  sinApprox_denom := Double((2 * sinApprox_n) * ((2 * sinApprox_n) + 1));
  sinApprox_term := ((-sinApprox_term * x) * x) / sinApprox_denom;
  sinApprox_sum := sinApprox_sum + sinApprox_term;
  sinApprox_n := sinApprox_n + 1;
end;
  exit(sinApprox_sum);
end;
function cosApprox(x: real): real;
var
  cosApprox_term: real;
  cosApprox_sum: real;
  cosApprox_n: integer;
  cosApprox_denom: real;
begin
  cosApprox_term := 1;
  cosApprox_sum := 1;
  cosApprox_n := 1;
  while cosApprox_n <= 10 do begin
  cosApprox_denom := Double(((2 * cosApprox_n) - 1) * (2 * cosApprox_n));
  cosApprox_term := ((-cosApprox_term * x) * x) / cosApprox_denom;
  cosApprox_sum := cosApprox_sum + cosApprox_term;
  cosApprox_n := cosApprox_n + 1;
end;
  exit(cosApprox_sum);
end;
function expApprox(x: real): real;
var
  expApprox_sum: real;
  expApprox_term: real;
  expApprox_n: integer;
begin
  expApprox_sum := 1;
  expApprox_term := 1;
  expApprox_n := 1;
  while expApprox_n < 10 do begin
  expApprox_term := (expApprox_term * x) / Double(expApprox_n);
  expApprox_sum := expApprox_sum + expApprox_term;
  expApprox_n := expApprox_n + 1;
end;
  exit(expApprox_sum);
end;
function ln_(x: real): real;
var
  ln__t: real;
  ln__term: real;
  ln__sum: real;
  ln__n: integer;
begin
  ln__t := (x - 1) / (x + 1);
  ln__term := ln__t;
  ln__sum := 0;
  ln__n := 1;
  while ln__n <= 19 do begin
  ln__sum := ln__sum + (ln__term / Double(ln__n));
  ln__term := (ln__term * ln__t) * ln__t;
  ln__n := ln__n + 2;
end;
  exit(2 * ln__sum);
end;
function log10(x: real): real;
begin
  exit(ln(x) div ln(10));
end;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrtApprox_guess := x;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 10 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function absf(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function normalize(audio: RealArray): RealArray;
var
  normalize_max_val: real;
  normalize_i: integer;
  normalize_v: real;
  normalize_res: array of real;
begin
  normalize_max_val := 0;
  normalize_i := 0;
  while normalize_i < Length(audio) do begin
  normalize_v := absf(audio[normalize_i]);
  if normalize_v > normalize_max_val then begin
  normalize_max_val := normalize_v;
end;
  normalize_i := normalize_i + 1;
end;
  normalize_res := [];
  normalize_i := 0;
  while normalize_i < Length(audio) do begin
  normalize_res := concat(normalize_res, [audio[normalize_i] / normalize_max_val]);
  normalize_i := normalize_i + 1;
end;
  exit(normalize_res);
end;
function dft(frame: RealArray; bins: integer): RealArray;
var
  dft_N: integer;
  dft_spec: array of real;
  dft_k: integer;
  dft_real_: real;
  dft_imag: real;
  dft_n_31: integer;
  dft_angle: real;
begin
  dft_N := Length(frame);
  dft_spec := [];
  dft_k := 0;
  while dft_k < bins do begin
  dft_real_ := 0;
  dft_imag := 0;
  dft_n_31 := 0;
  while dft_n_31 < dft_N do begin
  dft_angle := (((-2 * PI) * Double(dft_k)) * Double(dft_n_31)) / Double(dft_N);
  dft_real_ := dft_real_ + (frame[dft_n_31] * cosApprox(dft_angle));
  dft_imag := dft_imag + (frame[dft_n_31] * sinApprox(dft_angle));
  dft_n_31 := dft_n_31 + 1;
end;
  dft_spec := concat(dft_spec, [(dft_real_ * dft_real_) + (dft_imag * dft_imag)]);
  dft_k := dft_k + 1;
end;
  exit(dft_spec);
end;
function triangular_filters(bins: integer; spectrum_size: integer): RealArrayArray;
var
  triangular_filters_filters: array of RealArray;
  triangular_filters_b: integer;
  triangular_filters_center: integer;
  triangular_filters_filt: array of real;
  triangular_filters_i: integer;
  triangular_filters_v: real;
begin
  triangular_filters_filters := [];
  triangular_filters_b := 0;
  while triangular_filters_b < bins do begin
  triangular_filters_center := ((triangular_filters_b + 1) * spectrum_size) div (bins + 1);
  triangular_filters_filt := [];
  triangular_filters_i := 0;
  while triangular_filters_i < spectrum_size do begin
  triangular_filters_v := 0;
  if triangular_filters_i <= triangular_filters_center then begin
  triangular_filters_v := Double(triangular_filters_i) / Double(triangular_filters_center);
end else begin
  triangular_filters_v := Double(spectrum_size - triangular_filters_i) / Double(spectrum_size - triangular_filters_center);
end;
  triangular_filters_filt := concat(triangular_filters_filt, [triangular_filters_v]);
  triangular_filters_i := triangular_filters_i + 1;
end;
  triangular_filters_filters := concat(triangular_filters_filters, [triangular_filters_filt]);
  triangular_filters_b := triangular_filters_b + 1;
end;
  exit(triangular_filters_filters);
end;
function dot(mat: RealArrayArray; vec: RealArray): RealArray;
var
  dot_res: array of real;
  dot_i: integer;
  dot_sum: real;
  dot_j: integer;
begin
  dot_res := [];
  dot_i := 0;
  while dot_i < Length(mat) do begin
  dot_sum := 0;
  dot_j := 0;
  while dot_j < Length(vec) do begin
  dot_sum := dot_sum + (mat[dot_i][dot_j] * vec[dot_j]);
  dot_j := dot_j + 1;
end;
  dot_res := concat(dot_res, [dot_sum]);
  dot_i := dot_i + 1;
end;
  exit(dot_res);
end;
function discrete_cosine_transform(dct_filter_num: integer; filter_num: integer): RealArrayArray;
var
  discrete_cosine_transform_basis: array of RealArray;
  discrete_cosine_transform_i: integer;
  discrete_cosine_transform_row: array of real;
  discrete_cosine_transform_j: integer;
  discrete_cosine_transform_angle: real;
begin
  discrete_cosine_transform_basis := [];
  discrete_cosine_transform_i := 0;
  while discrete_cosine_transform_i < dct_filter_num do begin
  discrete_cosine_transform_row := [];
  discrete_cosine_transform_j := 0;
  while discrete_cosine_transform_j < filter_num do begin
  if discrete_cosine_transform_i = 0 then begin
  discrete_cosine_transform_row := concat(discrete_cosine_transform_row, [1 / sqrtApprox(Double(filter_num))]);
end else begin
  discrete_cosine_transform_angle := ((Double((2 * discrete_cosine_transform_j) + 1) * Double(discrete_cosine_transform_i)) * PI) / (2 * Double(filter_num));
  discrete_cosine_transform_row := concat(discrete_cosine_transform_row, [cosApprox(discrete_cosine_transform_angle) * sqrtApprox(2 / Double(filter_num))]);
end;
  discrete_cosine_transform_j := discrete_cosine_transform_j + 1;
end;
  discrete_cosine_transform_basis := concat(discrete_cosine_transform_basis, [discrete_cosine_transform_row]);
  discrete_cosine_transform_i := discrete_cosine_transform_i + 1;
end;
  exit(discrete_cosine_transform_basis);
end;
function mfcc(audio: RealArray; bins: integer; dct_num: integer): RealArray;
var
  mfcc_norm: RealArray;
  mfcc_spec: RealArray;
  mfcc_filters: RealArrayArray;
  mfcc_energies: RealArray;
  mfcc_logfb: array of real;
  mfcc_i: integer;
  mfcc_dct_basis: RealArrayArray;
  mfcc_res: RealArray;
begin
  mfcc_norm := normalize(audio);
  mfcc_spec := dft(mfcc_norm, bins + 2);
  mfcc_filters := triangular_filters(bins, Length(mfcc_spec));
  mfcc_energies := dot(mfcc_filters, mfcc_spec);
  mfcc_logfb := [];
  mfcc_i := 0;
  while mfcc_i < Length(mfcc_energies) do begin
  mfcc_logfb := concat(mfcc_logfb, [10 * log10(mfcc_energies[mfcc_i] + 1e-10)]);
  mfcc_i := mfcc_i + 1;
end;
  mfcc_dct_basis := discrete_cosine_transform(dct_num, bins);
  mfcc_res := dot(mfcc_dct_basis, mfcc_logfb);
  if Length(mfcc_res) = 0 then begin
  mfcc_res := [0, 0, 0];
end;
  exit(mfcc_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  sample_rate := 8000;
  size := 16;
  audio := [];
  n := 0;
  while n < size do begin
  t := Double(n) / Double(sample_rate);
  audio := concat(audio, [sinApprox(((2 * PI) * 440) * t)]);
  n := n + 1;
end;
  coeffs := mfcc(audio, 5, 3);
  for c in coeffs do begin
  writeln(c);
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
