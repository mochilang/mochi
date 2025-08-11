{$mode objfpc}
program Main;
uses SysUtils, Math;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  data: RealArray;
  x: real;
  n: integer;
  ndigits: integer;
function floor(x: real): real; forward;
function pow10(n: integer): real; forward;
function round(x: real; n: integer): real; forward;
function sqrtApprox(x: real): real; forward;
function mean(data: RealArray): real; forward;
function stdev(data: RealArray): real; forward;
function normalization(data: RealArray; ndigits: integer): RealArray; forward;
function standardization(data: RealArray; ndigits: integer): RealArray; forward;
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
  pow10_result_: real;
  pow10_i: integer;
begin
  pow10_result_ := 1;
  pow10_i := 0;
  while pow10_i < n do begin
  pow10_result_ := pow10_result_ * 10;
  pow10_i := pow10_i + 1;
end;
  exit(pow10_result_);
end;
function round(x: real; n: integer): real;
var
  round_m: real;
  round_y: real;
begin
  round_m := pow10(n);
  round_y := Double(Floor((x * round_m) + 0.5));
  exit(round_y / round_m);
end;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  sqrtApprox_guess := x;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function mean(data: RealArray): real;
var
  mean_total: real;
  mean_i: integer;
  mean_n: integer;
begin
  mean_total := 0;
  mean_i := 0;
  mean_n := Length(data);
  while mean_i < mean_n do begin
  mean_total := mean_total + data[mean_i];
  mean_i := mean_i + 1;
end;
  exit(mean_total / Double(mean_n));
end;
function stdev(data: RealArray): real;
var
  stdev_n: integer;
  stdev_m: real;
  stdev_sum_sq: real;
  stdev_i: integer;
  stdev_diff: real;
begin
  stdev_n := Length(data);
  if stdev_n <= 1 then begin
  panic('data length must be > 1');
end;
  stdev_m := mean(data);
  stdev_sum_sq := 0;
  stdev_i := 0;
  while stdev_i < stdev_n do begin
  stdev_diff := data[stdev_i] - stdev_m;
  stdev_sum_sq := stdev_sum_sq + (stdev_diff * stdev_diff);
  stdev_i := stdev_i + 1;
end;
  exit(sqrtApprox(stdev_sum_sq / Double(stdev_n - 1)));
end;
function normalization(data: RealArray; ndigits: integer): RealArray;
var
  normalization_x_min: real;
  normalization_x_max: real;
  normalization_denom: real;
  normalization_result_: array of real;
  normalization_i: integer;
  normalization_n: integer;
  normalization_norm: real;
begin
  normalization_x_min := Double(min_real(data));
  normalization_x_max := Double(max_real(data));
  normalization_denom := normalization_x_max - normalization_x_min;
  normalization_result_ := [];
  normalization_i := 0;
  normalization_n := Length(data);
  while normalization_i < normalization_n do begin
  normalization_norm := (data[normalization_i] - normalization_x_min) / normalization_denom;
  normalization_result_ := concat(normalization_result_, [round(normalization_norm, ndigits)]);
  normalization_i := normalization_i + 1;
end;
  exit(normalization_result_);
end;
function standardization(data: RealArray; ndigits: integer): RealArray;
var
  standardization_mu: real;
  standardization_sigma: real;
  standardization_result_: array of real;
  standardization_i: integer;
  standardization_n: integer;
  standardization_z: real;
begin
  standardization_mu := mean(data);
  standardization_sigma := stdev(data);
  standardization_result_ := [];
  standardization_i := 0;
  standardization_n := Length(data);
  while standardization_i < standardization_n do begin
  standardization_z := (data[standardization_i] - standardization_mu) / standardization_sigma;
  standardization_result_ := concat(standardization_result_, [round(standardization_z, ndigits)]);
  standardization_i := standardization_i + 1;
end;
  exit(standardization_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_real_to_str(normalization([2, 7, 10, 20, 30, 50], 3)));
  writeln(list_real_to_str(normalization([5, 10, 15, 20, 25], 3)));
  writeln(list_real_to_str(standardization([2, 7, 10, 20, 30, 50], 3)));
  writeln(list_real_to_str(standardization([5, 10, 15, 20, 25], 3)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
