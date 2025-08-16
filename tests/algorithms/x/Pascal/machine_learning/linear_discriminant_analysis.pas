{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
type IntArray = array of integer;
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
procedure show_list(xs: array of integer);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  PI: real;
  TWO_PI: real;
  seed: integer;
  total_count: integer;
  variance: real;
  items: RealArrayArray;
  class_count: integer;
  mean: real;
  m: real;
  means: RealArray;
  instance_count: integer;
  x: real;
  probabilities: RealArray;
  std_dev: real;
  x_items: RealArrayArray;
  actual_y: IntArray;
  predicted_y: IntArray;
function rand(): integer; forward;
function random(): real; forward;
function _mod(x: real; m: real): real; forward;
function cos(x: real): real; forward;
function sqrtApprox(x: real): real; forward;
function ln_(x: real): real; forward;
function gaussian_distribution(mean: real; std_dev: real; instance_count: integer): RealArray; forward;
function y_generator(class_count: integer; instance_count: IntArray): IntArray; forward;
function calculate_mean(instance_count: integer; items: RealArray): real; forward;
function calculate_probabilities(instance_count: integer; total_count: integer): real; forward;
function calculate_variance(items: RealArrayArray; means: RealArray; total_count: integer): real; forward;
function predict_y_values(x_items: RealArrayArray; means: RealArray; variance: real; probabilities: RealArray): IntArray; forward;
function accuracy(actual_y: IntArray; predicted_y: IntArray): real; forward;
procedure main(); forward;
function rand(): integer;
begin
  seed := ((seed * 1103515245) + 12345) mod 2147483648;
  exit(seed);
end;
function random(): real;
begin
  exit(Double(rand()) / 2.147483648e+09);
end;
function _mod(x: real; m: real): real;
begin
  exit(x - (Double(Trunc(x / m)) * m));
end;
function cos(x: real): real;
var
  cos_y: real;
  cos_y2: real;
  cos_y4: real;
  cos_y6: real;
begin
  cos_y := _mod(x + PI, TWO_PI) - PI;
  cos_y2 := cos_y * cos_y;
  cos_y4 := cos_y2 * cos_y2;
  cos_y6 := cos_y4 * cos_y2;
  exit(((1 - (cos_y2 / 2)) + (cos_y4 / 24)) - (cos_y6 / 720));
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
function gaussian_distribution(mean: real; std_dev: real; instance_count: integer): RealArray;
var
  gaussian_distribution_res: array of real;
  gaussian_distribution_i: integer;
  gaussian_distribution_u1: real;
  gaussian_distribution_u2: real;
  gaussian_distribution_r: real;
  gaussian_distribution_theta: real;
  gaussian_distribution_z: real;
begin
  gaussian_distribution_res := [];
  gaussian_distribution_i := 0;
  while gaussian_distribution_i < instance_count do begin
  gaussian_distribution_u1 := random();
  gaussian_distribution_u2 := random();
  gaussian_distribution_r := sqrtApprox(-2 * ln(gaussian_distribution_u1));
  gaussian_distribution_theta := TWO_PI * gaussian_distribution_u2;
  gaussian_distribution_z := gaussian_distribution_r * cos(gaussian_distribution_theta);
  gaussian_distribution_res := concat(gaussian_distribution_res, [mean + (gaussian_distribution_z * std_dev)]);
  gaussian_distribution_i := gaussian_distribution_i + 1;
end;
  exit(gaussian_distribution_res);
end;
function y_generator(class_count: integer; instance_count: IntArray): IntArray;
var
  y_generator_res: array of integer;
  y_generator_k: integer;
  y_generator_i: integer;
begin
  y_generator_res := [];
  y_generator_k := 0;
  while y_generator_k < class_count do begin
  y_generator_i := 0;
  while y_generator_i < instance_count[y_generator_k] do begin
  y_generator_res := concat(y_generator_res, IntArray([y_generator_k]));
  y_generator_i := y_generator_i + 1;
end;
  y_generator_k := y_generator_k + 1;
end;
  exit(y_generator_res);
end;
function calculate_mean(instance_count: integer; items: RealArray): real;
var
  calculate_mean_total: real;
  calculate_mean_i: integer;
begin
  calculate_mean_total := 0;
  calculate_mean_i := 0;
  while calculate_mean_i < instance_count do begin
  calculate_mean_total := calculate_mean_total + items[calculate_mean_i];
  calculate_mean_i := calculate_mean_i + 1;
end;
  exit(calculate_mean_total / Double(instance_count));
end;
function calculate_probabilities(instance_count: integer; total_count: integer): real;
begin
  exit(Double(instance_count) / Double(total_count));
end;
function calculate_variance(items: RealArrayArray; means: RealArray; total_count: integer): real;
var
  calculate_variance_squared_diff: array of real;
  calculate_variance_i: integer;
  calculate_variance_j: integer;
  calculate_variance_diff: real;
  calculate_variance_sum_sq: real;
  calculate_variance_k: integer;
  calculate_variance_n_classes: integer;
begin
  calculate_variance_squared_diff := [];
  calculate_variance_i := 0;
  while calculate_variance_i < Length(items) do begin
  calculate_variance_j := 0;
  while calculate_variance_j < Length(items[calculate_variance_i]) do begin
  calculate_variance_diff := items[calculate_variance_i][calculate_variance_j] - means[calculate_variance_i];
  calculate_variance_squared_diff := concat(calculate_variance_squared_diff, [calculate_variance_diff * calculate_variance_diff]);
  calculate_variance_j := calculate_variance_j + 1;
end;
  calculate_variance_i := calculate_variance_i + 1;
end;
  calculate_variance_sum_sq := 0;
  calculate_variance_k := 0;
  while calculate_variance_k < Length(calculate_variance_squared_diff) do begin
  calculate_variance_sum_sq := calculate_variance_sum_sq + calculate_variance_squared_diff[calculate_variance_k];
  calculate_variance_k := calculate_variance_k + 1;
end;
  calculate_variance_n_classes := Length(means);
  exit((1 / Double(total_count - calculate_variance_n_classes)) * calculate_variance_sum_sq);
end;
function predict_y_values(x_items: RealArrayArray; means: RealArray; variance: real; probabilities: RealArray): IntArray;
var
  predict_y_values_results: array of integer;
  predict_y_values_i: integer;
  predict_y_values_j: integer;
  predict_y_values_temp: array of real;
  predict_y_values_k: integer;
  predict_y_values_discr: real;
  predict_y_values_max_idx: integer;
  predict_y_values_max_val: real;
  predict_y_values_t: integer;
begin
  predict_y_values_results := [];
  predict_y_values_i := 0;
  while predict_y_values_i < Length(x_items) do begin
  predict_y_values_j := 0;
  while predict_y_values_j < Length(x_items[predict_y_values_i]) do begin
  predict_y_values_temp := [];
  predict_y_values_k := 0;
  while predict_y_values_k < Length(x_items) do begin
  predict_y_values_discr := ((x_items[predict_y_values_i][predict_y_values_j] * (means[predict_y_values_k] / variance)) - ((means[predict_y_values_k] * means[predict_y_values_k]) / (2 * variance))) + ln(probabilities[predict_y_values_k]);
  predict_y_values_temp := concat(predict_y_values_temp, [predict_y_values_discr]);
  predict_y_values_k := predict_y_values_k + 1;
end;
  predict_y_values_max_idx := 0;
  predict_y_values_max_val := predict_y_values_temp[0];
  predict_y_values_t := 1;
  while predict_y_values_t < Length(predict_y_values_temp) do begin
  if predict_y_values_temp[predict_y_values_t] > predict_y_values_max_val then begin
  predict_y_values_max_val := predict_y_values_temp[predict_y_values_t];
  predict_y_values_max_idx := predict_y_values_t;
end;
  predict_y_values_t := predict_y_values_t + 1;
end;
  predict_y_values_results := concat(predict_y_values_results, IntArray([predict_y_values_max_idx]));
  predict_y_values_j := predict_y_values_j + 1;
end;
  predict_y_values_i := predict_y_values_i + 1;
end;
  exit(predict_y_values_results);
end;
function accuracy(actual_y: IntArray; predicted_y: IntArray): real;
var
  accuracy_correct: integer;
  accuracy_i: integer;
begin
  accuracy_correct := 0;
  accuracy_i := 0;
  while accuracy_i < Length(actual_y) do begin
  if actual_y[accuracy_i] = predicted_y[accuracy_i] then begin
  accuracy_correct := accuracy_correct + 1;
end;
  accuracy_i := accuracy_i + 1;
end;
  exit((Double(accuracy_correct) / Double(Length(actual_y))) * 100);
end;
procedure main();
var
  main_counts: array of integer;
  main_means: array of real;
  main_std_dev: real;
  main_x: array of RealArray;
  main_i: integer;
  main_y: IntArray;
  main_actual_means: array of real;
  main_total_count: integer;
  main_probabilities: array of real;
  main_variance: real;
  main_predicted: IntArray;
begin
  seed := 1;
  main_counts := [20, 20, 20];
  main_means := [5, 10, 15];
  main_std_dev := 1;
  main_x := [];
  main_i := 0;
  while main_i < Length(main_counts) do begin
  main_x := concat(main_x, [gaussian_distribution(main_means[main_i], main_std_dev, main_counts[main_i])]);
  main_i := main_i + 1;
end;
  main_y := y_generator(Length(main_counts), main_counts);
  main_actual_means := [];
  main_i := 0;
  while main_i < Length(main_counts) do begin
  main_actual_means := concat(main_actual_means, [calculate_mean(main_counts[main_i], main_x[main_i])]);
  main_i := main_i + 1;
end;
  main_total_count := 0;
  main_i := 0;
  while main_i < Length(main_counts) do begin
  main_total_count := main_total_count + main_counts[main_i];
  main_i := main_i + 1;
end;
  main_probabilities := [];
  main_i := 0;
  while main_i < Length(main_counts) do begin
  main_probabilities := concat(main_probabilities, [calculate_probabilities(main_counts[main_i], main_total_count)]);
  main_i := main_i + 1;
end;
  main_variance := calculate_variance(main_x, main_actual_means, main_total_count);
  main_predicted := predict_y_values(main_x, main_actual_means, main_variance, main_probabilities);
  show_list(main_predicted);
  writeln(accuracy(main_y, main_predicted));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  TWO_PI := 6.283185307179586;
  seed := 1;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
