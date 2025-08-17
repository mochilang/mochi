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
procedure show_list_real(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
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
  sample: array of real;
function exp_taylor(exp_taylor_x: real): real; forward;
function sigmoid(sigmoid_vector: RealArray): RealArray; forward;
function gaussian_error_linear_unit(gaussian_error_linear_unit_vector: RealArray): RealArray; forward;
function exp_taylor(exp_taylor_x: real): real;
var
  exp_taylor_term: real;
  exp_taylor_sum: real;
  exp_taylor_i: real;
begin
  exp_taylor_term := 1;
  exp_taylor_sum := 1;
  exp_taylor_i := 1;
  while exp_taylor_i < 20 do begin
  exp_taylor_term := (exp_taylor_term * exp_taylor_x) / exp_taylor_i;
  exp_taylor_sum := exp_taylor_sum + exp_taylor_term;
  exp_taylor_i := exp_taylor_i + 1;
end;
  exit(exp_taylor_sum);
end;
function sigmoid(sigmoid_vector: RealArray): RealArray;
var
  sigmoid_result_: array of real;
  sigmoid_i: integer;
  sigmoid_x: real;
  sigmoid_value: real;
begin
  sigmoid_result_ := [];
  sigmoid_i := 0;
  while sigmoid_i < Length(sigmoid_vector) do begin
  sigmoid_x := sigmoid_vector[sigmoid_i];
  sigmoid_value := 1 / (1 + exp_taylor(-sigmoid_x));
  sigmoid_result_ := concat(sigmoid_result_, [sigmoid_value]);
  sigmoid_i := sigmoid_i + 1;
end;
  exit(sigmoid_result_);
end;
function gaussian_error_linear_unit(gaussian_error_linear_unit_vector: RealArray): RealArray;
var
  gaussian_error_linear_unit_result_: array of real;
  gaussian_error_linear_unit_i: integer;
  gaussian_error_linear_unit_x: real;
  gaussian_error_linear_unit_gelu: real;
begin
  gaussian_error_linear_unit_result_ := [];
  gaussian_error_linear_unit_i := 0;
  while gaussian_error_linear_unit_i < Length(gaussian_error_linear_unit_vector) do begin
  gaussian_error_linear_unit_x := gaussian_error_linear_unit_vector[gaussian_error_linear_unit_i];
  gaussian_error_linear_unit_gelu := gaussian_error_linear_unit_x * (1 / (1 + exp_taylor(-1.702 * gaussian_error_linear_unit_x)));
  gaussian_error_linear_unit_result_ := concat(gaussian_error_linear_unit_result_, [gaussian_error_linear_unit_gelu]);
  gaussian_error_linear_unit_i := gaussian_error_linear_unit_i + 1;
end;
  exit(gaussian_error_linear_unit_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  sample := [-1, 1, 2];
  show_list_real(sigmoid(sample));
  show_list_real(gaussian_error_linear_unit(sample));
  show_list_real(gaussian_error_linear_unit([-3]));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
