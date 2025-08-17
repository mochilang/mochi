{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type FuncType1 = function(arg0: real; arg1: real): real is nested;
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
  y2: RealArray;
  y3: RealArray;
  y4: RealArray;
  y5: RealArray;
  f: FuncType1;
  step_size: real;
  x: real;
  x_final: real;
  x_initials: RealArray;
  xs: RealArray;
  y: real;
  y_initials: RealArray;
function abs_float(x: real): real; forward;
procedure validate_inputs(x_initials: RealArray; step_size: real; x_final: real); forward;
function list_to_string(xs: RealArray): string; forward;
function adams_bashforth_step2(f: FuncType1; x_initials: RealArray; y_initials: RealArray; step_size: real; x_final: real): RealArray; forward;
function adams_bashforth_step3(f: FuncType1; x_initials: RealArray; y_initials: RealArray; step_size: real; x_final: real): RealArray; forward;
function adams_bashforth_step4(f: FuncType1; x_initials: RealArray; y_initials: RealArray; step_size: real; x_final: real): RealArray; forward;
function adams_bashforth_step5(f: FuncType1; x_initials: RealArray; y_initials: RealArray; step_size: real; x_final: real): RealArray; forward;
function f_x(x: real; y: real): real; forward;
function f_xy(x: real; y: real): real; forward;
function abs_float(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end else begin
  exit(x);
end;
end;
procedure validate_inputs(x_initials: RealArray; step_size: real; x_final: real);
var
  validate_inputs_i: integer;
  validate_inputs_diff: real;
begin
  if x_initials[Length(x_initials) - 1] >= x_final then begin
  panic('The final value of x must be greater than the initial values of x.');
end;
  if step_size <= 0 then begin
  panic('Step size must be positive.');
end;
  validate_inputs_i := 0;
  while validate_inputs_i < (Length(x_initials) - 1) do begin
  validate_inputs_diff := x_initials[validate_inputs_i + 1] - x_initials[validate_inputs_i];
  if abs_float(validate_inputs_diff - step_size) > 1e-10 then begin
  panic('x-values must be equally spaced according to step size.');
end;
  validate_inputs_i := validate_inputs_i + 1;
end;
end;
function list_to_string(xs: RealArray): string;
var
  list_to_string_s: string;
  list_to_string_i: integer;
begin
  list_to_string_s := '[';
  list_to_string_i := 0;
  while list_to_string_i < Length(xs) do begin
  list_to_string_s := list_to_string_s + FloatToStr(xs[list_to_string_i]);
  if (list_to_string_i + 1) < Length(xs) then begin
  list_to_string_s := list_to_string_s + ', ';
end;
  list_to_string_i := list_to_string_i + 1;
end;
  list_to_string_s := list_to_string_s + ']';
  exit(list_to_string_s);
end;
function adams_bashforth_step2(f: FuncType1; x_initials: RealArray; y_initials: RealArray; step_size: real; x_final: real): RealArray;
var
  adams_bashforth_step2_x0: real;
  adams_bashforth_step2_x1: real;
  adams_bashforth_step2_y: array of real;
  adams_bashforth_step2_n: integer;
  adams_bashforth_step2_i: integer;
  adams_bashforth_step2_term: real;
  adams_bashforth_step2_y_next: real;
begin
  validate_inputs(x_initials, step_size, x_final);
  if (Length(x_initials) <> 2) or (Length(y_initials) <> 2) then begin
  panic('Insufficient initial points information.');
end;
  adams_bashforth_step2_x0 := x_initials[0];
  adams_bashforth_step2_x1 := x_initials[1];
  adams_bashforth_step2_y := [];
  adams_bashforth_step2_y := concat(adams_bashforth_step2_y, [y_initials[0]]);
  adams_bashforth_step2_y := concat(adams_bashforth_step2_y, [y_initials[1]]);
  adams_bashforth_step2_n := Trunc((x_final - adams_bashforth_step2_x1) / step_size);
  adams_bashforth_step2_i := 0;
  while adams_bashforth_step2_i < adams_bashforth_step2_n do begin
  adams_bashforth_step2_term := (3 * f(adams_bashforth_step2_x1, adams_bashforth_step2_y[adams_bashforth_step2_i + 1])) - f(adams_bashforth_step2_x0, adams_bashforth_step2_y[adams_bashforth_step2_i]);
  adams_bashforth_step2_y_next := adams_bashforth_step2_y[adams_bashforth_step2_i + 1] + ((step_size / 2) * adams_bashforth_step2_term);
  adams_bashforth_step2_y := concat(adams_bashforth_step2_y, [adams_bashforth_step2_y_next]);
  adams_bashforth_step2_x0 := adams_bashforth_step2_x1;
  adams_bashforth_step2_x1 := adams_bashforth_step2_x1 + step_size;
  adams_bashforth_step2_i := adams_bashforth_step2_i + 1;
end;
  exit(adams_bashforth_step2_y);
end;
function adams_bashforth_step3(f: FuncType1; x_initials: RealArray; y_initials: RealArray; step_size: real; x_final: real): RealArray;
var
  adams_bashforth_step3_x0: real;
  adams_bashforth_step3_x1: real;
  adams_bashforth_step3_x2: real;
  adams_bashforth_step3_y: array of real;
  adams_bashforth_step3_n: integer;
  adams_bashforth_step3_i: integer;
  adams_bashforth_step3_term: real;
  adams_bashforth_step3_y_next: real;
begin
  validate_inputs(x_initials, step_size, x_final);
  if (Length(x_initials) <> 3) or (Length(y_initials) <> 3) then begin
  panic('Insufficient initial points information.');
end;
  adams_bashforth_step3_x0 := x_initials[0];
  adams_bashforth_step3_x1 := x_initials[1];
  adams_bashforth_step3_x2 := x_initials[2];
  adams_bashforth_step3_y := [];
  adams_bashforth_step3_y := concat(adams_bashforth_step3_y, [y_initials[0]]);
  adams_bashforth_step3_y := concat(adams_bashforth_step3_y, [y_initials[1]]);
  adams_bashforth_step3_y := concat(adams_bashforth_step3_y, [y_initials[2]]);
  adams_bashforth_step3_n := Trunc((x_final - adams_bashforth_step3_x2) / step_size);
  adams_bashforth_step3_i := 0;
  while adams_bashforth_step3_i <= adams_bashforth_step3_n do begin
  adams_bashforth_step3_term := ((23 * f(adams_bashforth_step3_x2, adams_bashforth_step3_y[adams_bashforth_step3_i + 2])) - (16 * f(adams_bashforth_step3_x1, adams_bashforth_step3_y[adams_bashforth_step3_i + 1]))) + (5 * f(adams_bashforth_step3_x0, adams_bashforth_step3_y[adams_bashforth_step3_i]));
  adams_bashforth_step3_y_next := adams_bashforth_step3_y[adams_bashforth_step3_i + 2] + ((step_size / 12) * adams_bashforth_step3_term);
  adams_bashforth_step3_y := concat(adams_bashforth_step3_y, [adams_bashforth_step3_y_next]);
  adams_bashforth_step3_x0 := adams_bashforth_step3_x1;
  adams_bashforth_step3_x1 := adams_bashforth_step3_x2;
  adams_bashforth_step3_x2 := adams_bashforth_step3_x2 + step_size;
  adams_bashforth_step3_i := adams_bashforth_step3_i + 1;
end;
  exit(adams_bashforth_step3_y);
end;
function adams_bashforth_step4(f: FuncType1; x_initials: RealArray; y_initials: RealArray; step_size: real; x_final: real): RealArray;
var
  adams_bashforth_step4_x0: real;
  adams_bashforth_step4_x1: real;
  adams_bashforth_step4_x2: real;
  adams_bashforth_step4_x3: real;
  adams_bashforth_step4_y: array of real;
  adams_bashforth_step4_n: integer;
  adams_bashforth_step4_i: integer;
  adams_bashforth_step4_term: real;
  adams_bashforth_step4_y_next: real;
begin
  validate_inputs(x_initials, step_size, x_final);
  if (Length(x_initials) <> 4) or (Length(y_initials) <> 4) then begin
  panic('Insufficient initial points information.');
end;
  adams_bashforth_step4_x0 := x_initials[0];
  adams_bashforth_step4_x1 := x_initials[1];
  adams_bashforth_step4_x2 := x_initials[2];
  adams_bashforth_step4_x3 := x_initials[3];
  adams_bashforth_step4_y := [];
  adams_bashforth_step4_y := concat(adams_bashforth_step4_y, [y_initials[0]]);
  adams_bashforth_step4_y := concat(adams_bashforth_step4_y, [y_initials[1]]);
  adams_bashforth_step4_y := concat(adams_bashforth_step4_y, [y_initials[2]]);
  adams_bashforth_step4_y := concat(adams_bashforth_step4_y, [y_initials[3]]);
  adams_bashforth_step4_n := Trunc((x_final - adams_bashforth_step4_x3) / step_size);
  adams_bashforth_step4_i := 0;
  while adams_bashforth_step4_i < adams_bashforth_step4_n do begin
  adams_bashforth_step4_term := (((55 * f(adams_bashforth_step4_x3, adams_bashforth_step4_y[adams_bashforth_step4_i + 3])) - (59 * f(adams_bashforth_step4_x2, adams_bashforth_step4_y[adams_bashforth_step4_i + 2]))) + (37 * f(adams_bashforth_step4_x1, adams_bashforth_step4_y[adams_bashforth_step4_i + 1]))) - (9 * f(adams_bashforth_step4_x0, adams_bashforth_step4_y[adams_bashforth_step4_i]));
  adams_bashforth_step4_y_next := adams_bashforth_step4_y[adams_bashforth_step4_i + 3] + ((step_size / 24) * adams_bashforth_step4_term);
  adams_bashforth_step4_y := concat(adams_bashforth_step4_y, [adams_bashforth_step4_y_next]);
  adams_bashforth_step4_x0 := adams_bashforth_step4_x1;
  adams_bashforth_step4_x1 := adams_bashforth_step4_x2;
  adams_bashforth_step4_x2 := adams_bashforth_step4_x3;
  adams_bashforth_step4_x3 := adams_bashforth_step4_x3 + step_size;
  adams_bashforth_step4_i := adams_bashforth_step4_i + 1;
end;
  exit(adams_bashforth_step4_y);
end;
function adams_bashforth_step5(f: FuncType1; x_initials: RealArray; y_initials: RealArray; step_size: real; x_final: real): RealArray;
var
  adams_bashforth_step5_x0: real;
  adams_bashforth_step5_x1: real;
  adams_bashforth_step5_x2: real;
  adams_bashforth_step5_x3: real;
  adams_bashforth_step5_x4: real;
  adams_bashforth_step5_y: array of real;
  adams_bashforth_step5_n: integer;
  adams_bashforth_step5_i: integer;
  adams_bashforth_step5_term: real;
  adams_bashforth_step5_y_next: real;
begin
  validate_inputs(x_initials, step_size, x_final);
  if (Length(x_initials) <> 5) or (Length(y_initials) <> 5) then begin
  panic('Insufficient initial points information.');
end;
  adams_bashforth_step5_x0 := x_initials[0];
  adams_bashforth_step5_x1 := x_initials[1];
  adams_bashforth_step5_x2 := x_initials[2];
  adams_bashforth_step5_x3 := x_initials[3];
  adams_bashforth_step5_x4 := x_initials[4];
  adams_bashforth_step5_y := [];
  adams_bashforth_step5_y := concat(adams_bashforth_step5_y, [y_initials[0]]);
  adams_bashforth_step5_y := concat(adams_bashforth_step5_y, [y_initials[1]]);
  adams_bashforth_step5_y := concat(adams_bashforth_step5_y, [y_initials[2]]);
  adams_bashforth_step5_y := concat(adams_bashforth_step5_y, [y_initials[3]]);
  adams_bashforth_step5_y := concat(adams_bashforth_step5_y, [y_initials[4]]);
  adams_bashforth_step5_n := Trunc((x_final - adams_bashforth_step5_x4) / step_size);
  adams_bashforth_step5_i := 0;
  while adams_bashforth_step5_i <= adams_bashforth_step5_n do begin
  adams_bashforth_step5_term := ((((1901 * f(adams_bashforth_step5_x4, adams_bashforth_step5_y[adams_bashforth_step5_i + 4])) - (2774 * f(adams_bashforth_step5_x3, adams_bashforth_step5_y[adams_bashforth_step5_i + 3]))) - (2616 * f(adams_bashforth_step5_x2, adams_bashforth_step5_y[adams_bashforth_step5_i + 2]))) - (1274 * f(adams_bashforth_step5_x1, adams_bashforth_step5_y[adams_bashforth_step5_i + 1]))) + (251 * f(adams_bashforth_step5_x0, adams_bashforth_step5_y[adams_bashforth_step5_i]));
  adams_bashforth_step5_y_next := adams_bashforth_step5_y[adams_bashforth_step5_i + 4] + ((step_size / 720) * adams_bashforth_step5_term);
  adams_bashforth_step5_y := concat(adams_bashforth_step5_y, [adams_bashforth_step5_y_next]);
  adams_bashforth_step5_x0 := adams_bashforth_step5_x1;
  adams_bashforth_step5_x1 := adams_bashforth_step5_x2;
  adams_bashforth_step5_x2 := adams_bashforth_step5_x3;
  adams_bashforth_step5_x3 := adams_bashforth_step5_x4;
  adams_bashforth_step5_x4 := adams_bashforth_step5_x4 + step_size;
  adams_bashforth_step5_i := adams_bashforth_step5_i + 1;
end;
  exit(adams_bashforth_step5_y);
end;
function f_x(x: real; y: real): real;
begin
  exit(x);
end;
function f_xy(x: real; y: real): real;
begin
  exit(x + y);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  y2 := adams_bashforth_step2(@f_x, [0, 0.2], [0, 0], 0.2, 1);
  writeln(list_to_string(y2));
  y3 := adams_bashforth_step3(@f_xy, [0, 0.2, 0.4], [0, 0, 0.04], 0.2, 1);
  writeln(FloatToStr(y3[3]));
  y4 := adams_bashforth_step4(@f_xy, [0, 0.2, 0.4, 0.6], [0, 0, 0.04, 0.128], 0.2, 1);
  writeln(FloatToStr(y4[4]));
  writeln(FloatToStr(y4[5]));
  y5 := adams_bashforth_step5(@f_xy, [0, 0.2, 0.4, 0.6, 0.8], [0, 0.0214, 0.0214, 0.22211, 0.42536], 0.2, 1);
  writeln(FloatToStr(y5[Length(y5) - 1]));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
