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
  capacitance: real;
  source_voltage: real;
  resistance: real;
  x: real;
  time_sec: real;
function expApprox(x: real): real; forward;
function round3(x: real): real; forward;
function charging_capacitor(source_voltage: real; resistance: real; capacitance: real; time_sec: real): real; forward;
function expApprox(x: real): real;
var
  expApprox_y: real;
  expApprox_is_neg: boolean;
  expApprox_term: real;
  expApprox_sum: real;
  expApprox_n: integer;
begin
  expApprox_y := x;
  expApprox_is_neg := false;
  if x < 0 then begin
  expApprox_is_neg := true;
  expApprox_y := -x;
end;
  expApprox_term := 1;
  expApprox_sum := 1;
  expApprox_n := 1;
  while expApprox_n < 30 do begin
  expApprox_term := (expApprox_term * expApprox_y) / Double(expApprox_n);
  expApprox_sum := expApprox_sum + expApprox_term;
  expApprox_n := expApprox_n + 1;
end;
  if expApprox_is_neg then begin
  exit(1 / expApprox_sum);
end;
  exit(expApprox_sum);
end;
function round3(x: real): real;
var
  round3_scaled: real;
  round3_scaled_int: integer;
begin
  round3_scaled := x * 1000;
  if round3_scaled >= 0 then begin
  round3_scaled := round3_scaled + 0.5;
end else begin
  round3_scaled := round3_scaled - 0.5;
end;
  round3_scaled_int := Trunc(round3_scaled);
  exit(Double(round3_scaled_int) / 1000);
end;
function charging_capacitor(source_voltage: real; resistance: real; capacitance: real; time_sec: real): real;
var
  charging_capacitor_exponent: real;
  charging_capacitor_voltage: real;
begin
  if source_voltage <= 0 then begin
  panic('Source voltage must be positive.');
end;
  if resistance <= 0 then begin
  panic('Resistance must be positive.');
end;
  if capacitance <= 0 then begin
  panic('Capacitance must be positive.');
end;
  charging_capacitor_exponent := -time_sec / (resistance * capacitance);
  charging_capacitor_voltage := source_voltage * (1 - expApprox(charging_capacitor_exponent));
  exit(round3(charging_capacitor_voltage));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(charging_capacitor(0.2, 0.9, 8.4, 0.5));
  writeln(charging_capacitor(2.2, 3.5, 2.4, 9));
  writeln(charging_capacitor(15, 200, 20, 2));
  writeln(charging_capacitor(20, 2000, 0.0003, 4));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
