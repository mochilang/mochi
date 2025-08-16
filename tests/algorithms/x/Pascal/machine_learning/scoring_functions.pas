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
  x: real;
  actual: RealArray;
  predict: RealArray;
function absf(x: real): real; forward;
function sqrtApprox(x: real): real; forward;
function ln_series(x: real): real; forward;
function ln_(x: real): real; forward;
function mae(predict: RealArray; actual: RealArray): real; forward;
function mse(predict: RealArray; actual: RealArray): real; forward;
function rmse(predict: RealArray; actual: RealArray): real; forward;
function rmsle(predict: RealArray; actual: RealArray): real; forward;
function mbd(predict: RealArray; actual: RealArray): real; forward;
function manual_accuracy(predict: RealArray; actual: RealArray): real; forward;
procedure main(); forward;
function absf(x: real): real;
begin
  if x < 0 then begin
  exit(0 - x);
end;
  exit(x);
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
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function ln_series(x: real): real;
var
  ln_series_t: real;
  ln_series_term: real;
  ln_series_sum: real;
  ln_series_n: integer;
begin
  ln_series_t := (x - 1) / (x + 1);
  ln_series_term := ln_series_t;
  ln_series_sum := 0;
  ln_series_n := 1;
  while ln_series_n <= 19 do begin
  ln_series_sum := ln_series_sum + (ln_series_term / Double(ln_series_n));
  ln_series_term := (ln_series_term * ln_series_t) * ln_series_t;
  ln_series_n := ln_series_n + 2;
end;
  exit(2 * ln_series_sum);
end;
function ln_(x: real): real;
var
  ln__y: real;
  ln__k: integer;
begin
  ln__y := x;
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
function mae(predict: RealArray; actual: RealArray): real;
var
  mae_sum: real;
  mae_i: integer;
  mae_diff: real;
begin
  mae_sum := 0;
  mae_i := 0;
  while mae_i < Length(predict) do begin
  mae_diff := predict[mae_i] - actual[mae_i];
  mae_sum := mae_sum + absf(mae_diff);
  mae_i := mae_i + 1;
end;
  exit(mae_sum / Double(Length(predict)));
end;
function mse(predict: RealArray; actual: RealArray): real;
var
  mse_sum: real;
  mse_i: integer;
  mse_diff: real;
begin
  mse_sum := 0;
  mse_i := 0;
  while mse_i < Length(predict) do begin
  mse_diff := predict[mse_i] - actual[mse_i];
  mse_sum := mse_sum + (mse_diff * mse_diff);
  mse_i := mse_i + 1;
end;
  exit(mse_sum / Double(Length(predict)));
end;
function rmse(predict: RealArray; actual: RealArray): real;
begin
  exit(sqrtApprox(mse(predict, actual)));
end;
function rmsle(predict: RealArray; actual: RealArray): real;
var
  rmsle_sum: real;
  rmsle_i: integer;
  rmsle_lp: real;
  rmsle_la: real;
  rmsle_diff: real;
begin
  rmsle_sum := 0;
  rmsle_i := 0;
  while rmsle_i < Length(predict) do begin
  rmsle_lp := ln(predict[rmsle_i] + 1);
  rmsle_la := ln(actual[rmsle_i] + 1);
  rmsle_diff := rmsle_lp - rmsle_la;
  rmsle_sum := rmsle_sum + (rmsle_diff * rmsle_diff);
  rmsle_i := rmsle_i + 1;
end;
  exit(sqrtApprox(rmsle_sum / Double(Length(predict))));
end;
function mbd(predict: RealArray; actual: RealArray): real;
var
  mbd_diff_sum: real;
  mbd_actual_sum: real;
  mbd_i: integer;
  mbd_n: real;
  mbd_numerator: real;
  mbd_denominator: real;
begin
  mbd_diff_sum := 0;
  mbd_actual_sum := 0;
  mbd_i := 0;
  while mbd_i < Length(predict) do begin
  mbd_diff_sum := mbd_diff_sum + (predict[mbd_i] - actual[mbd_i]);
  mbd_actual_sum := mbd_actual_sum + actual[mbd_i];
  mbd_i := mbd_i + 1;
end;
  mbd_n := Double(Length(predict));
  mbd_numerator := mbd_diff_sum / mbd_n;
  mbd_denominator := mbd_actual_sum / mbd_n;
  exit((mbd_numerator / mbd_denominator) * 100);
end;
function manual_accuracy(predict: RealArray; actual: RealArray): real;
var
  manual_accuracy_correct: integer;
  manual_accuracy_i: integer;
begin
  manual_accuracy_correct := 0;
  manual_accuracy_i := 0;
  while manual_accuracy_i < Length(predict) do begin
  if predict[manual_accuracy_i] = actual[manual_accuracy_i] then begin
  manual_accuracy_correct := manual_accuracy_correct + 1;
end;
  manual_accuracy_i := manual_accuracy_i + 1;
end;
  exit(Double(manual_accuracy_correct) / Double(Length(predict)));
end;
procedure main();
var
  main_actual: array of real;
  main_predict: array of real;
begin
  main_actual := [1, 2, 3];
  main_predict := [1, 4, 3];
  writeln(FloatToStr(mae(main_predict, main_actual)));
  writeln(FloatToStr(mse(main_predict, main_actual)));
  writeln(FloatToStr(rmse(main_predict, main_actual)));
  writeln(FloatToStr(rmsle([10, 2, 30], [10, 10, 30])));
  writeln(FloatToStr(mbd([2, 3, 4], [1, 2, 3])));
  writeln(FloatToStr(mbd([0, 1, 1], [1, 2, 3])));
  writeln(FloatToStr(manual_accuracy(main_predict, main_actual)));
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
