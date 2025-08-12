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
  x: real;
  vector: RealArray;
function exp_approx(x: real): real; forward;
function ln_series(x: real): real; forward;
function ln(x: real): real; forward;
function softplus(x: real): real; forward;
function tanh_approx(x: real): real; forward;
function mish(vector: RealArray): RealArray; forward;
procedure main(); forward;
function exp_approx(x: real): real;
var
  exp_approx_neg: boolean;
  exp_approx_y: real;
  exp_approx_term: real;
  exp_approx_sum: real;
  exp_approx_n: integer;
begin
  exp_approx_neg := false;
  exp_approx_y := x;
  if x < 0 then begin
  exp_approx_neg := true;
  exp_approx_y := -x;
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
function ln_series(x: real): real;
var
  ln_series_t: real;
  ln_series_term: real;
  ln_series_acc: real;
  ln_series_n: integer;
begin
  ln_series_t := (x - 1) / (x + 1);
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
function ln(x: real): real;
var
  ln_y: real;
  ln_k: integer;
begin
  ln_y := x;
  ln_k := 0;
  while ln_y >= 10 do begin
  ln_y := ln_y / 10;
  ln_k := ln_k + 1;
end;
  while ln_y < 1 do begin
  ln_y := ln_y * 10;
  ln_k := ln_k - 1;
end;
  exit(ln_series(ln_y) + (Double(ln_k) * ln_series(10)));
end;
function softplus(x: real): real;
begin
  exit(ln(1 + exp_approx(x)));
end;
function tanh_approx(x: real): real;
begin
  exit((2 / (1 + exp_approx(-2 * x))) - 1);
end;
function mish(vector: RealArray): RealArray;
var
  mish_result_: array of real;
  mish_i: integer;
  mish_x: real;
  mish_sp: real;
  mish_y: real;
begin
  mish_result_ := [];
  mish_i := 0;
  while mish_i < Length(vector) do begin
  mish_x := vector[mish_i];
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
end.
