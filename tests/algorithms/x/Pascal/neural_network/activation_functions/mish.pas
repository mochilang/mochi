{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
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
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
function list_real_to_str(xs: array of real): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + FloatToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function exp_approx(exp_approx_x: real): real; forward;
function ln_series(ln_series_x: real): real; forward;
function ln_(ln__x: real): real; forward;
function softplus(softplus_x: real): real; forward;
function tanh_approx(tanh_approx_x: real): real; forward;
function mish(mish_vector: RealArray): RealArray; forward;
procedure main(); forward;
function exp_approx(exp_approx_x: real): real;
var
  exp_approx_neg: boolean;
  exp_approx_y: real;
  exp_approx_term: real;
  exp_approx_sum: real;
  exp_approx_n: int64;
begin
  exp_approx_neg := false;
  exp_approx_y := exp_approx_x;
  if exp_approx_x < 0 then begin
  exp_approx_neg := true;
  exp_approx_y := -exp_approx_x;
end;
  exp_approx_term := 1;
  exp_approx_sum := 1;
  exp_approx_n := 1;
  while exp_approx_n < 30 do begin
  exp_approx_term := (exp_approx_term * exp_approx_y) / Double(exp_approx_n);
  exp_approx_sum := exp_approx_sum + exp_approx_term;
  exp_approx_n := exp_approx_n + 1;
end;
  if exp_approx_neg then begin
  exit(1 / exp_approx_sum);
end;
  exit(exp_approx_sum);
end;
function ln_series(ln_series_x: real): real;
var
  ln_series_t: real;
  ln_series_term: real;
  ln_series_acc: real;
  ln_series_n: int64;
begin
  ln_series_t := (ln_series_x - 1) / (ln_series_x + 1);
  ln_series_term := ln_series_t;
  ln_series_acc := 0;
  ln_series_n := 1;
  while ln_series_n <= 19 do begin
  ln_series_acc := ln_series_acc + (ln_series_term / Double(ln_series_n));
  ln_series_term := (ln_series_term * ln_series_t) * ln_series_t;
  ln_series_n := ln_series_n + 2;
end;
  exit(2 * ln_series_acc);
end;
function ln_(ln__x: real): real;
var
  ln__y: real;
  ln__k: int64;
begin
  ln__y := ln__x;
  ln__k := 0;
  while ln__y >= 10 do begin
  ln__y := ln__y / 10;
  ln__k := ln__k + 1;
end;
  while ln__y < 1 do begin
  ln__y := ln__y * 10;
  ln__k := ln__k - 1;
end;
  exit(ln_series(ln__y) + (Double(ln__k) * ln_series(10)));
end;
function softplus(softplus_x: real): real;
begin
  exit(ln(1 + exp_approx(softplus_x)));
end;
function tanh_approx(tanh_approx_x: real): real;
begin
  exit((2 / (1 + exp_approx(-2 * tanh_approx_x))) - 1);
end;
function mish(mish_vector: RealArray): RealArray;
var
  mish_result_: array of real;
  mish_i: int64;
  mish_x: real;
  mish_sp: real;
  mish_y: real;
begin
  mish_result_ := [];
  mish_i := 0;
  while mish_i < Length(mish_vector) do begin
  mish_x := mish_vector[mish_i];
  mish_sp := softplus(mish_x);
  mish_y := mish_x * tanh_approx(mish_sp);
  mish_result_ := concat(mish_result_, [mish_y]);
  mish_i := mish_i + 1;
end;
  exit(mish_result_);
end;
procedure main();
var
  main_v1: array of real;
  main_v2: array of real;
begin
  main_v1 := [2.3, 0.6, -2, -3.8];
  main_v2 := [-9.2, -0.3, 0.45, -4.56];
  writeln(list_real_to_str(mish(main_v1)));
  writeln(list_real_to_str(mish(main_v2)));
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
  writeln('');
end.
