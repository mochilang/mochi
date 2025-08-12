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
  vector: RealArray;
  x: real;
  lambda_: real;
  alpha: real;
function exp(x: real): real; forward;
function scaled_exponential_linear_unit(vector: RealArray; alpha: real; lambda_: real): RealArray; forward;
function exp(x: real): real;
var
  exp_term: real;
  exp_sum: real;
  exp_n: integer;
begin
  exp_term := 1;
  exp_sum := 1;
  exp_n := 1;
  while exp_n < 20 do begin
  exp_term := (exp_term * x) / Double(exp_n);
  exp_sum := exp_sum + exp_term;
  exp_n := exp_n + 1;
end;
  exit(exp_sum);
end;
function scaled_exponential_linear_unit(vector: RealArray; alpha: real; lambda_: real): RealArray;
var
  scaled_exponential_linear_unit_result_: array of real;
  scaled_exponential_linear_unit_i: integer;
  scaled_exponential_linear_unit_x: real;
  scaled_exponential_linear_unit_y: real;
begin
  scaled_exponential_linear_unit_result_ := [];
  scaled_exponential_linear_unit_i := 0;
  while scaled_exponential_linear_unit_i < Length(vector) do begin
  scaled_exponential_linear_unit_x := vector[scaled_exponential_linear_unit_i];
  if scaled_exponential_linear_unit_x > 0 then begin
  scaled_exponential_linear_unit_y := lambda_ * scaled_exponential_linear_unit_x;
end else begin
  scaled_exponential_linear_unit_y := (lambda_ * alpha) * (exp(scaled_exponential_linear_unit_x) - 1);
end;
  scaled_exponential_linear_unit_result_ := concat(scaled_exponential_linear_unit_result_, [scaled_exponential_linear_unit_y]);
  scaled_exponential_linear_unit_i := scaled_exponential_linear_unit_i + 1;
end;
  exit(scaled_exponential_linear_unit_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_real_to_str(scaled_exponential_linear_unit([1.3, 3.7, 2.4], 1.6732, 1.0507)));
  writeln(list_real_to_str(scaled_exponential_linear_unit([1.3, 4.7, 8.2], 1.6732, 1.0507)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
