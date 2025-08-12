{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
type RealArray = array of real;
type IntArray = array of integer;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  y: integer;
  probabilities: RealArray;
  values: IntArray;
  y_probabilities: RealArray;
  x_probabilities: RealArray;
  y_values: IntArray;
  x: real;
  v: real;
  x_values: IntArray;
function key(x: integer; y: integer): string; forward;
function joint_probability_distribution(x_values: IntArray; y_values: IntArray; x_probabilities: RealArray; y_probabilities: RealArray): specialize TFPGMap<string, real>; forward;
function expectation(values: IntArray; probabilities: RealArray): real; forward;
function variance(values: IntArray; probabilities: RealArray): real; forward;
function covariance(x_values: IntArray; y_values: IntArray; x_probabilities: RealArray; y_probabilities: RealArray): real; forward;
function sqrtApprox(x: real): real; forward;
function standard_deviation(v: real): real; forward;
procedure main(); forward;
function key(x: integer; y: integer): string;
begin
  exit((IntToStr(x) + ',') + IntToStr(y));
end;
function joint_probability_distribution(x_values: IntArray; y_values: IntArray; x_probabilities: RealArray; y_probabilities: RealArray): specialize TFPGMap<string, real>;
var
  joint_probability_distribution_result_: specialize TFPGMap<string, real>;
  joint_probability_distribution_i: integer;
  joint_probability_distribution_j: integer;
  joint_probability_distribution_k: string;
begin
  joint_probability_distribution_result_ := specialize TFPGMap<string, real>.Create();
  joint_probability_distribution_i := 0;
  while joint_probability_distribution_i < Length(x_values) do begin
  joint_probability_distribution_j := 0;
  while joint_probability_distribution_j < Length(y_values) do begin
  joint_probability_distribution_k := key(x_values[joint_probability_distribution_i], y_values[joint_probability_distribution_j]);
  joint_probability_distribution_result_[joint_probability_distribution_k] := x_probabilities[joint_probability_distribution_i] * y_probabilities[joint_probability_distribution_j];
  joint_probability_distribution_j := joint_probability_distribution_j + 1;
end;
  joint_probability_distribution_i := joint_probability_distribution_i + 1;
end;
  exit(joint_probability_distribution_result_);
end;
function expectation(values: IntArray; probabilities: RealArray): real;
var
  expectation_total: real;
  expectation_i: integer;
begin
  expectation_total := 0;
  expectation_i := 0;
  while expectation_i < Length(values) do begin
  expectation_total := expectation_total + (Double(values[expectation_i]) * probabilities[expectation_i]);
  expectation_i := expectation_i + 1;
end;
  exit(expectation_total);
end;
function variance(values: IntArray; probabilities: RealArray): real;
var
  variance_mean: real;
  variance_total: real;
  variance_i: integer;
  variance_diff: real;
begin
  variance_mean := expectation(values, probabilities);
  variance_total := 0;
  variance_i := 0;
  while variance_i < Length(values) do begin
  variance_diff := Double(values[variance_i]) - variance_mean;
  variance_total := variance_total + ((variance_diff * variance_diff) * probabilities[variance_i]);
  variance_i := variance_i + 1;
end;
  exit(variance_total);
end;
function covariance(x_values: IntArray; y_values: IntArray; x_probabilities: RealArray; y_probabilities: RealArray): real;
var
  covariance_mean_x: real;
  covariance_mean_y: real;
  covariance_total: real;
  covariance_i: integer;
  covariance_j: integer;
  covariance_diff_x: real;
  covariance_diff_y: real;
begin
  covariance_mean_x := expectation(x_values, x_probabilities);
  covariance_mean_y := expectation(y_values, y_probabilities);
  covariance_total := 0;
  covariance_i := 0;
  while covariance_i < Length(x_values) do begin
  covariance_j := 0;
  while covariance_j < Length(y_values) do begin
  covariance_diff_x := Double(x_values[covariance_i]) - covariance_mean_x;
  covariance_diff_y := Double(y_values[covariance_j]) - covariance_mean_y;
  covariance_total := covariance_total + (((covariance_diff_x * covariance_diff_y) * x_probabilities[covariance_i]) * y_probabilities[covariance_j]);
  covariance_j := covariance_j + 1;
end;
  covariance_i := covariance_i + 1;
end;
  exit(covariance_total);
end;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrtApprox_guess := x / 2;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function standard_deviation(v: real): real;
begin
  exit(sqrtApprox(v));
end;
procedure main();
var
  main_x_values: array of integer;
  main_y_values: array of integer;
  main_x_probabilities: array of real;
  main_y_probabilities: array of real;
  main_jpd: specialize TFPGMap<string, real>;
  main_i: integer;
  main_j: integer;
  main_k: string;
  main_prob: real;
  main_prob_idx: integer;
  main_ex: real;
  main_ey: real;
  main_vx: real;
  main_vy: real;
  main_cov: real;
begin
  main_x_values := [1, 2];
  main_y_values := [-2, 5, 8];
  main_x_probabilities := [0.7, 0.3];
  main_y_probabilities := [0.3, 0.5, 0.2];
  main_jpd := joint_probability_distribution(main_x_values, main_y_values, main_x_probabilities, main_y_probabilities);
  main_i := 0;
  while main_i < Length(main_x_values) do begin
  main_j := 0;
  while main_j < Length(main_y_values) do begin
  main_k := key(main_x_values[main_i], main_y_values[main_j]);
  main_prob_idx := main_jpd.IndexOf(main_k);
  if main_prob_idx <> -1 then begin
  main_prob := main_jpd.Data[main_prob_idx];
end else begin
  main_prob := 0;
end;
  writeln((main_k + '=') + FloatToStr(main_prob));
  main_j := main_j + 1;
end;
  main_i := main_i + 1;
end;
  main_ex := expectation(main_x_values, main_x_probabilities);
  main_ey := expectation(main_y_values, main_y_probabilities);
  main_vx := variance(main_x_values, main_x_probabilities);
  main_vy := variance(main_y_values, main_y_probabilities);
  main_cov := covariance(main_x_values, main_y_values, main_x_probabilities, main_y_probabilities);
  writeln('Ex=' + FloatToStr(main_ex));
  writeln('Ey=' + FloatToStr(main_ey));
  writeln('Vx=' + FloatToStr(main_vx));
  writeln('Vy=' + FloatToStr(main_vy));
  writeln('Cov=' + FloatToStr(main_cov));
  writeln('Sx=' + FloatToStr(standard_deviation(main_vx)));
  writeln('Sy=' + FloatToStr(standard_deviation(main_vy)));
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
