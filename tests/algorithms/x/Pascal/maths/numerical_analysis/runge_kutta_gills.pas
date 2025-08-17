{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
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
  y1: RealArray;
  y2: RealArray;
  func: FuncType1;
  step_size: real;
  x: real;
  x_final: real;
  x_initial: real;
  y: real;
  y_initial: real;
function sqrt(x: real): real; forward;
function runge_kutta_gills(func: FuncType1; x_initial: real; y_initial: real; step_size: real; x_final: real): RealArray; forward;
function f1(x: real; y: real): real; forward;
function f2(x: real; y: real): real; forward;
function sqrt(x: real): real;
var
  sqrt_guess: real;
  sqrt_i: integer;
begin
  sqrt_guess := IfThen(x > 1, x / 2, 1);
  sqrt_i := 0;
  while sqrt_i < 20 do begin
  sqrt_guess := 0.5 * (sqrt_guess + (x / sqrt_guess));
  sqrt_i := sqrt_i + 1;
end;
  exit(sqrt_guess);
end;
function runge_kutta_gills(func: FuncType1; x_initial: real; y_initial: real; step_size: real; x_final: real): RealArray;
var
  runge_kutta_gills_n: integer;
  runge_kutta_gills_y: array of real;
  runge_kutta_gills_i: integer;
  runge_kutta_gills_xi: real;
  runge_kutta_gills_idx: integer;
  runge_kutta_gills_root2: real;
  runge_kutta_gills_k1: real;
  runge_kutta_gills_k2: real;
  runge_kutta_gills_k3: real;
  runge_kutta_gills_k4: real;
begin
  if x_initial >= x_final then begin
  panic('The final value of x must be greater than initial value of x.');
end;
  if step_size <= 0 then begin
  panic('Step size must be positive.');
end;
  runge_kutta_gills_n := Trunc((x_final - x_initial) / step_size);
  runge_kutta_gills_y := [];
  runge_kutta_gills_i := 0;
  while runge_kutta_gills_i <= runge_kutta_gills_n do begin
  runge_kutta_gills_y := concat(runge_kutta_gills_y, [0]);
  runge_kutta_gills_i := runge_kutta_gills_i + 1;
end;
  runge_kutta_gills_y[0] := y_initial;
  runge_kutta_gills_xi := x_initial;
  runge_kutta_gills_idx := 0;
  runge_kutta_gills_root2 := sqrt(2);
  while runge_kutta_gills_idx < runge_kutta_gills_n do begin
  runge_kutta_gills_k1 := step_size * func(runge_kutta_gills_xi, runge_kutta_gills_y[runge_kutta_gills_idx]);
  runge_kutta_gills_k2 := step_size * func(runge_kutta_gills_xi + (step_size / 2), runge_kutta_gills_y[runge_kutta_gills_idx] + (runge_kutta_gills_k1 / 2));
  runge_kutta_gills_k3 := step_size * func(runge_kutta_gills_xi + (step_size / 2), (runge_kutta_gills_y[runge_kutta_gills_idx] + ((-0.5 + (1 / runge_kutta_gills_root2)) * runge_kutta_gills_k1)) + ((1 - (1 / runge_kutta_gills_root2)) * runge_kutta_gills_k2));
  runge_kutta_gills_k4 := step_size * func(runge_kutta_gills_xi + step_size, (runge_kutta_gills_y[runge_kutta_gills_idx] - ((1 / runge_kutta_gills_root2) * runge_kutta_gills_k2)) + ((1 + (1 / runge_kutta_gills_root2)) * runge_kutta_gills_k3));
  runge_kutta_gills_y[runge_kutta_gills_idx + 1] := runge_kutta_gills_y[runge_kutta_gills_idx] + ((((runge_kutta_gills_k1 + ((2 - runge_kutta_gills_root2) * runge_kutta_gills_k2)) + ((2 + runge_kutta_gills_root2) * runge_kutta_gills_k3)) + runge_kutta_gills_k4) / 6);
  runge_kutta_gills_xi := runge_kutta_gills_xi + step_size;
  runge_kutta_gills_idx := runge_kutta_gills_idx + 1;
end;
  exit(runge_kutta_gills_y);
end;
function f1(x: real; y: real): real;
begin
  exit((x - y) / 2);
end;
function f2(x: real; y: real): real;
begin
  exit(x);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  y1 := runge_kutta_gills(@f1, 0, 3, 0.2, 5);
  writeln(FloatToStr(y1[Length(y1) - 1]));
  y2 := runge_kutta_gills(@f2, -1, 0, 0.2, 0);
  writeln(list_real_to_str(y2));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
