{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  G: real;
function sqrt(sqrt_x: real): real; forward;
function terminal_velocity(terminal_velocity_mass: real; terminal_velocity_density: real; terminal_velocity_area: real; terminal_velocity_drag_coefficient: real): real; forward;
function sqrt(sqrt_x: real): real;
var
  sqrt_guess: real;
  sqrt_i: int64;
begin
  if sqrt_x <= 0 then begin
  exit(0);
end;
  sqrt_guess := sqrt_x;
  sqrt_i := 0;
  while sqrt_i < 10 do begin
  sqrt_guess := (sqrt_guess + (sqrt_x / sqrt_guess)) / 2;
  sqrt_i := sqrt_i + 1;
end;
  exit(sqrt_guess);
end;
function terminal_velocity(terminal_velocity_mass: real; terminal_velocity_density: real; terminal_velocity_area: real; terminal_velocity_drag_coefficient: real): real;
var
  terminal_velocity_numerator: real;
  terminal_velocity_denominator: real;
  terminal_velocity_result_: real;
begin
  if (((terminal_velocity_mass <= 0) or (terminal_velocity_density <= 0)) or (terminal_velocity_area <= 0)) or (terminal_velocity_drag_coefficient <= 0) then begin
  panic('mass, density, area and the drag coefficient all need to be positive');
end;
  terminal_velocity_numerator := (2 * terminal_velocity_mass) * G;
  terminal_velocity_denominator := (terminal_velocity_density * terminal_velocity_area) * terminal_velocity_drag_coefficient;
  terminal_velocity_result_ := sqrt(terminal_velocity_numerator / terminal_velocity_denominator);
  exit(terminal_velocity_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  G := 9.80665;
  writeln(FloatToStr(terminal_velocity(1, 25, 0.6, 0.77)));
  writeln(FloatToStr(terminal_velocity(2, 100, 0.45, 0.23)));
  writeln(FloatToStr(terminal_velocity(5, 50, 0.2, 0.5)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
