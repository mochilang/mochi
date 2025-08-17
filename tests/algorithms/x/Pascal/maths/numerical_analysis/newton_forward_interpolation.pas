{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  x_points: array of real;
  y_points: array of real;
  n: integer;
  p: integer;
  u: real;
  value: real;
  x: RealArray;
  y0: RealArray;
function ucal(u: real; p: integer): real; forward;
function factorial(n: integer): real; forward;
function newton_forward_interpolation(x: RealArray; y0: RealArray; value: real): real; forward;
function ucal(u: real; p: integer): real;
var
  ucal_temp: real;
  ucal_i: integer;
begin
  ucal_temp := u;
  ucal_i := 1;
  while ucal_i < p do begin
  ucal_temp := ucal_temp * (u - Double(ucal_i));
  ucal_i := ucal_i + 1;
end;
  exit(ucal_temp);
end;
function factorial(n: integer): real;
var
  factorial_result_: real;
  factorial_i: integer;
begin
  factorial_result_ := 1;
  factorial_i := 2;
  while factorial_i <= n do begin
  factorial_result_ := factorial_result_ * Double(factorial_i);
  factorial_i := factorial_i + 1;
end;
  exit(factorial_result_);
end;
function newton_forward_interpolation(x: RealArray; y0: RealArray; value: real): real;
var
  newton_forward_interpolation_n: integer;
  newton_forward_interpolation_y: array of RealArray;
  newton_forward_interpolation_i: integer;
  newton_forward_interpolation_row: array of real;
  newton_forward_interpolation_j: integer;
  newton_forward_interpolation_i1: integer;
  newton_forward_interpolation_j1: integer;
  newton_forward_interpolation_u: real;
  newton_forward_interpolation_sum: real;
  newton_forward_interpolation_k: integer;
begin
  newton_forward_interpolation_n := Length(x);
  newton_forward_interpolation_y := [];
  newton_forward_interpolation_i := 0;
  while newton_forward_interpolation_i < newton_forward_interpolation_n do begin
  newton_forward_interpolation_row := [];
  newton_forward_interpolation_j := 0;
  while newton_forward_interpolation_j < newton_forward_interpolation_n do begin
  newton_forward_interpolation_row := concat(newton_forward_interpolation_row, [0]);
  newton_forward_interpolation_j := newton_forward_interpolation_j + 1;
end;
  newton_forward_interpolation_y := concat(newton_forward_interpolation_y, [newton_forward_interpolation_row]);
  newton_forward_interpolation_i := newton_forward_interpolation_i + 1;
end;
  newton_forward_interpolation_i := 0;
  while newton_forward_interpolation_i < newton_forward_interpolation_n do begin
  newton_forward_interpolation_y[newton_forward_interpolation_i][0] := y0[newton_forward_interpolation_i];
  newton_forward_interpolation_i := newton_forward_interpolation_i + 1;
end;
  newton_forward_interpolation_i1 := 1;
  while newton_forward_interpolation_i1 < newton_forward_interpolation_n do begin
  newton_forward_interpolation_j1 := 0;
  while newton_forward_interpolation_j1 < (newton_forward_interpolation_n - newton_forward_interpolation_i1) do begin
  newton_forward_interpolation_y[newton_forward_interpolation_j1][newton_forward_interpolation_i1] := newton_forward_interpolation_y[newton_forward_interpolation_j1 + 1][newton_forward_interpolation_i1 - 1] - newton_forward_interpolation_y[newton_forward_interpolation_j1][newton_forward_interpolation_i1 - 1];
  newton_forward_interpolation_j1 := newton_forward_interpolation_j1 + 1;
end;
  newton_forward_interpolation_i1 := newton_forward_interpolation_i1 + 1;
end;
  newton_forward_interpolation_u := (value - x[0]) / (x[1] - x[0]);
  newton_forward_interpolation_sum := newton_forward_interpolation_y[0][0];
  newton_forward_interpolation_k := 1;
  while newton_forward_interpolation_k < newton_forward_interpolation_n do begin
  newton_forward_interpolation_sum := newton_forward_interpolation_sum + ((ucal(newton_forward_interpolation_u, newton_forward_interpolation_k) * newton_forward_interpolation_y[0][newton_forward_interpolation_k]) / factorial(newton_forward_interpolation_k));
  newton_forward_interpolation_k := newton_forward_interpolation_k + 1;
end;
  exit(newton_forward_interpolation_sum);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  x_points := [0, 1, 2, 3];
  y_points := [0, 1, 8, 27];
  writeln(FloatToStr(newton_forward_interpolation(x_points, y_points, 1.5)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
