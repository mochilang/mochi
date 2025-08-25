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
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
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
function sqrt(sqrt_x: real): real; forward;
function real_power(real_power_apparent_power: real; real_power_power_factor: real): real; forward;
function reactive_power(reactive_power_apparent_power: real; reactive_power_power_factor: real): real; forward;
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
function real_power(real_power_apparent_power: real; real_power_power_factor: real): real;
begin
  if (real_power_power_factor < (0 - 1)) or (real_power_power_factor > 1) then begin
  panic('power_factor must be a valid float value between -1 and 1.');
end;
  exit(real_power_apparent_power * real_power_power_factor);
end;
function reactive_power(reactive_power_apparent_power: real; reactive_power_power_factor: real): real;
begin
  if (reactive_power_power_factor < (0 - 1)) or (reactive_power_power_factor > 1) then begin
  panic('power_factor must be a valid float value between -1 and 1.');
end;
  exit(reactive_power_apparent_power * sqrt(1 - (reactive_power_power_factor * reactive_power_power_factor)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(FloatToStr(real_power(100, 0.9)));
  writeln(FloatToStr(real_power(0, 0.8)));
  writeln(FloatToStr(real_power(100, -0.9)));
  writeln(FloatToStr(reactive_power(100, 0.9)));
  writeln(FloatToStr(reactive_power(0, 0.8)));
  writeln(FloatToStr(reactive_power(100, -0.9)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
