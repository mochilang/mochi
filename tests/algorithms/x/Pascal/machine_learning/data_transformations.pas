{$mode objfpc}
program Main;
uses SysUtils, Math;
type RealArray = array of real;
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
function min_real(xs: array of real): real;
var i: integer; m: real;
begin
  if Length(xs) = 0 then begin min_real := 0; exit; end;
  m := xs[0];
  for i := 1 to High(xs) do if xs[i] < m then m := xs[i];
  min_real := m;
end;
function max_real(xs: array of real): real;
var i: integer; m: real;
begin
  if Length(xs) = 0 then begin max_real := 0; exit; end;
  m := xs[0];
  for i := 1 to High(xs) do if xs[i] > m then m := xs[i];
  max_real := m;
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
function floor(floor_x: real): real; forward;
function pow10(pow10_n: integer): real; forward;
function round(round_x: real; round_n: integer): real; forward;
function sqrtApprox(sqrtApprox_x: real): real; forward;
function mean(mean_data: RealArray): real; forward;
function stdev(stdev_data: RealArray): real; forward;
function normalization(normalization_data: RealArray; normalization_ndigits: integer): RealArray; forward;
function standardization(standardization_data: RealArray; standardization_ndigits: integer): RealArray; forward;
function floor(floor_x: real): real;
var
  floor_i: integer;
begin
  floor_i := Trunc(floor_x);
  if Double(floor_i) > floor_x then begin
  floor_i := floor_i - 1;
end;
  exit(Double(floor_i));
end;
function pow10(pow10_n: integer): real;
var
  pow10_result_: real;
  pow10_i: integer;
begin
  pow10_result_ := 1;
  pow10_i := 0;
  while pow10_i < pow10_n do begin
  pow10_result_ := pow10_result_ * 10;
  pow10_i := pow10_i + 1;
end;
  exit(pow10_result_);
end;
function round(round_x: real; round_n: integer): real;
var
  round_m: real;
  round_y: real;
begin
  round_m := pow10(round_n);
  round_y := Double(Floor((round_x * round_m) + 0.5));
  exit(round_y / round_m);
end;
function sqrtApprox(sqrtApprox_x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  sqrtApprox_guess := sqrtApprox_x;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (sqrtApprox_x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function mean(mean_data: RealArray): real;
var
  mean_total: real;
  mean_i: integer;
  mean_n: integer;
begin
  mean_total := 0;
  mean_i := 0;
  mean_n := Length(mean_data);
  while mean_i < mean_n do begin
  mean_total := mean_total + mean_data[mean_i];
  mean_i := mean_i + 1;
end;
  exit(mean_total / Double(mean_n));
end;
function stdev(stdev_data: RealArray): real;
var
  stdev_n: integer;
  stdev_m: real;
  stdev_sum_sq: real;
  stdev_i: integer;
  stdev_diff: real;
begin
  stdev_n := Length(stdev_data);
  if stdev_n <= 1 then begin
  panic('data length must be > 1');
end;
  stdev_m := mean(stdev_data);
  stdev_sum_sq := 0;
  stdev_i := 0;
  while stdev_i < stdev_n do begin
  stdev_diff := stdev_data[stdev_i] - stdev_m;
  stdev_sum_sq := stdev_sum_sq + (stdev_diff * stdev_diff);
  stdev_i := stdev_i + 1;
end;
  exit(sqrtApprox(stdev_sum_sq / Double(stdev_n - 1)));
end;
function normalization(normalization_data: RealArray; normalization_ndigits: integer): RealArray;
var
  normalization_x_min: real;
  normalization_x_max: real;
  normalization_denom: real;
  normalization_result_: array of real;
  normalization_i: integer;
  normalization_n: integer;
  normalization_norm: real;
begin
  normalization_x_min := Double(min_real(normalization_data));
  normalization_x_max := Double(max_real(normalization_data));
  normalization_denom := normalization_x_max - normalization_x_min;
  normalization_result_ := [];
  normalization_i := 0;
  normalization_n := Length(normalization_data);
  while normalization_i < normalization_n do begin
  normalization_norm := (normalization_data[normalization_i] - normalization_x_min) / normalization_denom;
  normalization_result_ := concat(normalization_result_, [round(normalization_norm, normalization_ndigits)]);
  normalization_i := normalization_i + 1;
end;
  exit(normalization_result_);
end;
function standardization(standardization_data: RealArray; standardization_ndigits: integer): RealArray;
var
  standardization_mu: real;
  standardization_sigma: real;
  standardization_result_: array of real;
  standardization_i: integer;
  standardization_n: integer;
  standardization_z: real;
begin
  standardization_mu := mean(standardization_data);
  standardization_sigma := stdev(standardization_data);
  standardization_result_ := [];
  standardization_i := 0;
  standardization_n := Length(standardization_data);
  while standardization_i < standardization_n do begin
  standardization_z := (standardization_data[standardization_i] - standardization_mu) / standardization_sigma;
  standardization_result_ := concat(standardization_result_, [round(standardization_z, standardization_ndigits)]);
  standardization_i := standardization_i + 1;
end;
  exit(standardization_result_);
end;
begin
  writeln(list_real_to_str(normalization([2, 7, 10, 20, 30, 50], 3)));
  writeln(list_real_to_str(normalization([5, 10, 15, 20, 25], 3)));
  writeln(list_real_to_str(standardization([2, 7, 10, 20, 30, 50], 3)));
  writeln(list_real_to_str(standardization([5, 10, 15, 20, 25], 3)));
end.
