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
  BOLTZMANN: real;
  ELECTRON_VOLT: real;
  TEMPERATURE: real;
  acceptor_conc: real;
  n: integer;
  intrinsic_conc: real;
  donor_conc: real;
  x: real;
function pow10(n: integer): real; forward;
function ln_series(x: real): real; forward;
function ln(x: real): real; forward;
function builtin_voltage(donor_conc: real; acceptor_conc: real; intrinsic_conc: real): real; forward;
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
function ln_series(x: real): real;
var
  ln_series_t: real;
  ln_series_term: real;
  ln_series_sum: real;
  ln_series_n: integer;
begin
  ln_series_t := (x - 1) / (x + 1);
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
function ln(x: real): real;
var
  ln_y: real;
  ln_k: integer;
begin
  ln_y := x;
  ln_k := 0;
  while ln_y >= 10 do begin
  ln_y := ln_y / 10;
  ln_k := ln_k + 1;
end;
  while ln_y < 1 do begin
  ln_y := ln_y * 10;
  ln_k := ln_k - 1;
end;
  exit(ln_series(ln_y) + (Double(ln_k) * ln_series(10)));
end;
function builtin_voltage(donor_conc: real; acceptor_conc: real; intrinsic_conc: real): real;
begin
  if donor_conc <= 0 then begin
  panic('Donor concentration should be positive');
end;
  if acceptor_conc <= 0 then begin
  panic('Acceptor concentration should be positive');
end;
  if intrinsic_conc <= 0 then begin
  panic('Intrinsic concentration should be positive');
end;
  if donor_conc <= intrinsic_conc then begin
  panic('Donor concentration should be greater than intrinsic concentration');
end;
  if acceptor_conc <= intrinsic_conc then begin
  panic('Acceptor concentration should be greater than intrinsic concentration');
end;
  exit(((BOLTZMANN * TEMPERATURE) * ln((donor_conc * acceptor_conc) / (intrinsic_conc * intrinsic_conc))) / ELECTRON_VOLT);
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
end.
