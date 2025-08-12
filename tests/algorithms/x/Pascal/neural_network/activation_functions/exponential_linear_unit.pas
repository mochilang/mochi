{$mode objfpc}{$modeswitch nestedprocvars}
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
  alpha: real;
  vector: RealArray;
function exp_approx(x: real): real; forward;
function exponential_linear_unit(vector: RealArray; alpha: real): RealArray; forward;
function exp_approx(x: real): real;
var
  exp_approx_sum: real;
  exp_approx_term: real;
  exp_approx_i: integer;
  exp_approx_absx: real;
begin
  exp_approx_sum := 1;
  exp_approx_term := 1;
  exp_approx_i := 1;
  if x < 0 then begin
  exp_approx_absx := -x;
end else begin
  exp_approx_absx := x;
end;
  while exp_approx_i <= 20 do begin
  exp_approx_term := (exp_approx_term * exp_approx_absx) / Double(exp_approx_i);
  exp_approx_sum := exp_approx_sum + exp_approx_term;
  exp_approx_i := exp_approx_i + 1;
end;
  if x < 0 then begin
  exit(1 / exp_approx_sum);
end;
  exit(exp_approx_sum);
end;
function exponential_linear_unit(vector: RealArray; alpha: real): RealArray;
var
  exponential_linear_unit_result_: array of real;
  exponential_linear_unit_i: integer;
  exponential_linear_unit_v: real;
  exponential_linear_unit_neg: real;
begin
  exponential_linear_unit_result_ := [];
  exponential_linear_unit_i := 0;
  while exponential_linear_unit_i < Length(vector) do begin
  exponential_linear_unit_v := vector[exponential_linear_unit_i];
  if exponential_linear_unit_v > 0 then begin
  exponential_linear_unit_result_ := concat(exponential_linear_unit_result_, [exponential_linear_unit_v]);
end else begin
  exponential_linear_unit_neg := alpha * (exp_approx(exponential_linear_unit_v) - 1);
  exponential_linear_unit_result_ := concat(exponential_linear_unit_result_, [exponential_linear_unit_neg]);
end;
  exponential_linear_unit_i := exponential_linear_unit_i + 1;
end;
  exit(exponential_linear_unit_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_real_to_str(exponential_linear_unit([2.3, 0.6, -2, -3.8], 0.3)));
  writeln(list_real_to_str(exponential_linear_unit([-9.2, -0.3, 0.45, -4.56], 0.067)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
