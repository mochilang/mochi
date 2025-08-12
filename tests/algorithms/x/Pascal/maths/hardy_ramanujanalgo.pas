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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  n: integer;
  x: real;
function exact_prime_factor_count(n: integer): integer; forward;
function ln(x: real): real; forward;
function floor(x: real): real; forward;
function round4(x: real): real; forward;
procedure main(); forward;
function exact_prime_factor_count(n: integer): integer;
var
  exact_prime_factor_count_count: integer;
  exact_prime_factor_count_num: integer;
  exact_prime_factor_count_i: integer;
begin
  exact_prime_factor_count_count := 0;
  exact_prime_factor_count_num := n;
  if (exact_prime_factor_count_num mod 2) = 0 then begin
  exact_prime_factor_count_count := exact_prime_factor_count_count + 1;
  while (exact_prime_factor_count_num mod 2) = 0 do begin
  exact_prime_factor_count_num := exact_prime_factor_count_num div 2;
end;
end;
  exact_prime_factor_count_i := 3;
  while (exact_prime_factor_count_i * exact_prime_factor_count_i) <= exact_prime_factor_count_num do begin
  if (exact_prime_factor_count_num mod exact_prime_factor_count_i) = 0 then begin
  exact_prime_factor_count_count := exact_prime_factor_count_count + 1;
  while (exact_prime_factor_count_num mod exact_prime_factor_count_i) = 0 do begin
  exact_prime_factor_count_num := exact_prime_factor_count_num div exact_prime_factor_count_i;
end;
end;
  exact_prime_factor_count_i := exact_prime_factor_count_i + 2;
end;
  if exact_prime_factor_count_num > 2 then begin
  exact_prime_factor_count_count := exact_prime_factor_count_count + 1;
end;
  exit(exact_prime_factor_count_count);
end;
function ln(x: real): real;
var
  ln_ln2: real;
  ln_y: real;
  ln_k: real;
  ln_t: real;
  ln_term: real;
  ln_sum: real;
  ln_n: integer;
begin
  ln_ln2 := 0.6931471805599453;
  ln_y := x;
  ln_k := 0;
  while ln_y > 2 do begin
  ln_y := ln_y / 2;
  ln_k := ln_k + ln_ln2;
end;
  while ln_y < 1 do begin
  ln_y := ln_y * 2;
  ln_k := ln_k - ln_ln2;
end;
  ln_t := (ln_y - 1) / (ln_y + 1);
  ln_term := ln_t;
  ln_sum := 0;
  ln_n := 1;
  while ln_n <= 19 do begin
  ln_sum := ln_sum + (ln_term / Double(ln_n));
  ln_term := (ln_term * ln_t) * ln_t;
  ln_n := ln_n + 2;
end;
  exit(ln_k + (2 * ln_sum));
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
function round4(x: real): real;
var
  round4_m: real;
begin
  round4_m := 10000;
  exit(Floor((x * round4_m) + 0.5) / round4_m);
end;
procedure main();
var
  main_n: integer;
  main_count: integer;
  main_loglog: real;
begin
  main_n := 51242183;
  main_count := exact_prime_factor_count(main_n);
  writeln('The number of distinct prime factors is/are ' + IntToStr(main_count));
  main_loglog := ln(ln(Double(main_n)));
  writeln('The value of log(log(n)) is ' + FloatToStr(round4(main_loglog)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
