{$mode objfpc}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  time: real;
  source_voltage: real;
  resistance: real;
  x: real;
  n: integer;
  inductance: real;
function expApprox(x: real): real; forward;
function floor(x: real): real; forward;
function pow10(n: integer): real; forward;
function round(x: real; n: integer): real; forward;
function charging_inductor(source_voltage: real; resistance: real; inductance: real; time: real): real; forward;
function expApprox(x: real): real;
var
  expApprox_half: real;
  expApprox_sum: real;
  expApprox_term: real;
  expApprox_n: integer;
begin
  if x < 0 then begin
  exit(1 / expApprox(-x));
end;
  if x > 1 then begin
  expApprox_half := expApprox(x / 2);
  exit(expApprox_half * expApprox_half);
end;
  expApprox_sum := 1;
  expApprox_term := 1;
  expApprox_n := 1;
  while expApprox_n < 20 do begin
  expApprox_term := (expApprox_term * x) / Double(expApprox_n);
  expApprox_sum := expApprox_sum + expApprox_term;
  expApprox_n := expApprox_n + 1;
end;
  exit(expApprox_sum);
end;
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
begin
  round_m := pow10(n);
  exit(floor((x * round_m) + 0.5) / round_m);
end;
function charging_inductor(source_voltage: real; resistance: real; inductance: real; time: real): real;
var
  charging_inductor_exponent: real;
  charging_inductor_current: real;
begin
  if source_voltage <= 0 then begin
  panic('Source voltage must be positive.');
end;
  if resistance <= 0 then begin
  panic('Resistance must be positive.');
end;
  if inductance <= 0 then begin
  panic('Inductance must be positive.');
end;
  charging_inductor_exponent := (-time * resistance) / inductance;
  charging_inductor_current := (source_voltage / resistance) * (1 - expApprox(charging_inductor_exponent));
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
end.
