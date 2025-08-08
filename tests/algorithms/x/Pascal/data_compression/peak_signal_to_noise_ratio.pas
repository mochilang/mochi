{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
  contrast: IntArrayArray;
  x: real;
  original: IntArrayArray;
function abs(x: real): real; forward;
function sqrtApprox(x: real): real; forward;
function ln(x: real): real; forward;
function log10(x: real): real; forward;
function peak_signal_to_noise_ratio(original: IntArrayArray; contrast: IntArrayArray): real; forward;
function abs(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrtApprox_guess := x;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 10 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function ln(x: real): real;
var
  ln_t: real;
  ln_term: real;
  ln_sum: real;
  ln_n: integer;
begin
  ln_t := (x - 1) / (x + 1);
  ln_term := ln_t;
  ln_sum := 0;
  ln_n := 1;
  while ln_n <= 19 do begin
  ln_sum := ln_sum + (ln_term / Double(ln_n));
  ln_term := (ln_term * ln_t) * ln_t;
  ln_n := ln_n + 2;
end;
  exit(2 * ln_sum);
end;
function log10(x: real): real;
begin
  exit(ln(x) / ln(10));
end;
function peak_signal_to_noise_ratio(original: IntArrayArray; contrast: IntArrayArray): real;
var
  peak_signal_to_noise_ratio_mse: real;
  peak_signal_to_noise_ratio_i: integer;
  peak_signal_to_noise_ratio_j: integer;
  peak_signal_to_noise_ratio_diff: real;
  peak_signal_to_noise_ratio_size: real;
  peak_signal_to_noise_ratio_PIXEL_MAX: real;
begin
  peak_signal_to_noise_ratio_mse := 0;
  peak_signal_to_noise_ratio_i := 0;
  while peak_signal_to_noise_ratio_i < Length(original) do begin
  peak_signal_to_noise_ratio_j := 0;
  while peak_signal_to_noise_ratio_j < Length(original[peak_signal_to_noise_ratio_i]) do begin
  peak_signal_to_noise_ratio_diff := Double(original[peak_signal_to_noise_ratio_i][peak_signal_to_noise_ratio_j] - contrast[peak_signal_to_noise_ratio_i][peak_signal_to_noise_ratio_j]);
  peak_signal_to_noise_ratio_mse := peak_signal_to_noise_ratio_mse + (peak_signal_to_noise_ratio_diff * peak_signal_to_noise_ratio_diff);
  peak_signal_to_noise_ratio_j := peak_signal_to_noise_ratio_j + 1;
end;
  peak_signal_to_noise_ratio_i := peak_signal_to_noise_ratio_i + 1;
end;
  peak_signal_to_noise_ratio_size := Double(Length(original) * Length(original[0]));
  peak_signal_to_noise_ratio_mse := peak_signal_to_noise_ratio_mse / peak_signal_to_noise_ratio_size;
  if peak_signal_to_noise_ratio_mse = 0 then begin
  exit(100);
end;
  peak_signal_to_noise_ratio_PIXEL_MAX := 255;
  exit(20 * log10(peak_signal_to_noise_ratio_PIXEL_MAX / sqrtApprox(peak_signal_to_noise_ratio_mse)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
