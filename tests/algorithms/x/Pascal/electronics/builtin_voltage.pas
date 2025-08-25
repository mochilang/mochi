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
  BOLTZMANN: real;
  ELECTRON_VOLT: real;
  TEMPERATURE: real;
function pow10(pow10_n: int64): real; forward;
function ln_series(ln_series_x: real): real; forward;
function ln_(ln__x: real): real; forward;
function builtin_voltage(builtin_voltage_donor_conc: real; builtin_voltage_acceptor_conc: real; builtin_voltage_intrinsic_conc: real): real; forward;
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
function ln_series(ln_series_x: real): real;
var
  ln_series_t: real;
  ln_series_term: real;
  ln_series_sum: real;
  ln_series_n: int64;
begin
  ln_series_t := (ln_series_x - 1) / (ln_series_x + 1);
  ln_series_term := ln_series_t;
  ln_series_sum := 0;
  ln_series_n := 1;
  while ln_series_n <= 19 do begin
  ln_series_sum := ln_series_sum + (ln_series_term / Double(ln_series_n));
  ln_series_term := (ln_series_term * ln_series_t) * ln_series_t;
  ln_series_n := ln_series_n + 2;
end;
  exit(2 * ln_series_sum);
end;
function ln_(ln__x: real): real;
var
  ln__y: real;
  ln__k: int64;
begin
  ln__y := ln__x;
  ln__k := 0;
  while ln__y >= 10 do begin
  ln__y := ln__y / 10;
  ln__k := ln__k + 1;
end;
  while ln__y < 1 do begin
  ln__y := ln__y * 10;
  ln__k := ln__k - 1;
end;
  exit(ln_series(ln__y) + (Double(ln__k) * ln_series(10)));
end;
function builtin_voltage(builtin_voltage_donor_conc: real; builtin_voltage_acceptor_conc: real; builtin_voltage_intrinsic_conc: real): real;
begin
  if builtin_voltage_donor_conc <= 0 then begin
  panic('Donor concentration should be positive');
end;
  if builtin_voltage_acceptor_conc <= 0 then begin
  panic('Acceptor concentration should be positive');
end;
  if builtin_voltage_intrinsic_conc <= 0 then begin
  panic('Intrinsic concentration should be positive');
end;
  if builtin_voltage_donor_conc <= builtin_voltage_intrinsic_conc then begin
  panic('Donor concentration should be greater than intrinsic concentration');
end;
  if builtin_voltage_acceptor_conc <= builtin_voltage_intrinsic_conc then begin
  panic('Acceptor concentration should be greater than intrinsic concentration');
end;
  exit(((BOLTZMANN * TEMPERATURE) * ln((builtin_voltage_donor_conc * builtin_voltage_acceptor_conc) / (builtin_voltage_intrinsic_conc * builtin_voltage_intrinsic_conc))) / ELECTRON_VOLT);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  BOLTZMANN := 1.380649 / pow10(23);
  ELECTRON_VOLT := 1.602176634 / pow10(19);
  TEMPERATURE := 300;
  writeln(FloatToStr(builtin_voltage(pow10(17), pow10(17), pow10(10))));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
