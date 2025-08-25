{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
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
function expApprox(expApprox_x: real): real; forward;
function floor(floor_x: real): real; forward;
function pow10(pow10_n: int64): real; forward;
function round(round_x: real; round_n: int64): real; forward;
function charging_inductor(charging_inductor_source_voltage: real; charging_inductor_resistance: real; charging_inductor_inductance: real; charging_inductor_time: real): real; forward;
function expApprox(expApprox_x: real): real;
var
  expApprox_half: real;
  expApprox_sum: real;
  expApprox_term: real;
  expApprox_n: int64;
begin
  if expApprox_x < 0 then begin
  exit(1 / expApprox(-expApprox_x));
end;
  if expApprox_x > 1 then begin
  expApprox_half := expApprox(expApprox_x / 2);
  exit(expApprox_half * expApprox_half);
end;
  expApprox_sum := 1;
  expApprox_term := 1;
  expApprox_n := 1;
  while expApprox_n < 20 do begin
  expApprox_term := (expApprox_term * expApprox_x) / Double(expApprox_n);
  expApprox_sum := expApprox_sum + expApprox_term;
  expApprox_n := expApprox_n + 1;
end;
  exit(expApprox_sum);
end;
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
function pow10(pow10_n: int64): real;
var
  pow10_result_: real;
  pow10_i: int64;
begin
  pow10_result_ := 1;
  pow10_i := 0;
  while pow10_i < pow10_n do begin
  pow10_result_ := pow10_result_ * 10;
  pow10_i := pow10_i + 1;
end;
  exit(pow10_result_);
end;
function round(round_x: real; round_n: int64): real;
var
  round_m: real;
begin
  round_m := pow10(round_n);
  exit(Floor((round_x * round_m) + 0.5) / round_m);
end;
function charging_inductor(charging_inductor_source_voltage: real; charging_inductor_resistance: real; charging_inductor_inductance: real; charging_inductor_time: real): real;
var
  charging_inductor_exponent: real;
  charging_inductor_current: real;
begin
  if charging_inductor_source_voltage <= 0 then begin
  panic('Source voltage must be positive.');
end;
  if charging_inductor_resistance <= 0 then begin
  panic('Resistance must be positive.');
end;
  if charging_inductor_inductance <= 0 then begin
  panic('Inductance must be positive.');
end;
  charging_inductor_exponent := (-charging_inductor_time * charging_inductor_resistance) / charging_inductor_inductance;
  charging_inductor_current := (charging_inductor_source_voltage / charging_inductor_resistance) * (1 - expApprox(charging_inductor_exponent));
  exit(round(charging_inductor_current, 3));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(charging_inductor(5.8, 1.5, 2.3, 2));
  writeln(charging_inductor(8, 5, 3, 2));
  writeln(charging_inductor(8, 5 * pow10(2), 3, 2));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
