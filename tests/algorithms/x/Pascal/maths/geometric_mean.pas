{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
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
  nums: RealArray;
  x: real;
  exp: integer;
  base: real;
function abs(x: real): real; forward;
function pow_int(base: real; exp: integer): real; forward;
function nth_root(x: real; n: integer): real; forward;
function round_nearest(x: real): real; forward;
function compute_geometric_mean(nums: RealArray): real; forward;
procedure test_compute_geometric_mean(); forward;
procedure main(); forward;
function abs(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function pow_int(base: real; exp: integer): real;
var
  pow_int_result_: real;
  pow_int_i: integer;
begin
  pow_int_result_ := 1;
  pow_int_i := 0;
  while pow_int_i < exp do begin
  pow_int_result_ := pow_int_result_ * base;
  pow_int_i := pow_int_i + 1;
end;
  exit(pow_int_result_);
end;
function nth_root(x: real; n: integer): real;
var
  nth_root_guess: real;
  nth_root_i: integer;
  nth_root_denom: real;
begin
  if x = 0 then begin
  exit(0);
end;
  nth_root_guess := x;
  nth_root_i := 0;
  while nth_root_i < 10 do begin
  nth_root_denom := pow_int(nth_root_guess, n - 1);
  nth_root_guess := ((Double(n - 1) * nth_root_guess) + (x / nth_root_denom)) / Double(n);
  nth_root_i := nth_root_i + 1;
end;
  exit(nth_root_guess);
end;
function round_nearest(x: real): real;
var
  round_nearest_n: integer;
begin
  if x >= 0 then begin
  round_nearest_n := Trunc(x + 0.5);
  exit(Double(round_nearest_n));
end;
  round_nearest_n := Trunc(x - 0.5);
  exit(Double(round_nearest_n));
end;
function compute_geometric_mean(nums: RealArray): real;
var
  compute_geometric_mean_product: real;
  compute_geometric_mean_i: integer;
  compute_geometric_mean_mean: real;
  compute_geometric_mean_possible: real;
begin
  if Length(nums) = 0 then begin
  panic('no numbers');
end;
  compute_geometric_mean_product := 1;
  compute_geometric_mean_i := 0;
  while compute_geometric_mean_i < Length(nums) do begin
  compute_geometric_mean_product := compute_geometric_mean_product * nums[compute_geometric_mean_i];
  compute_geometric_mean_i := compute_geometric_mean_i + 1;
end;
  if (compute_geometric_mean_product < 0) and ((Length(nums) mod 2) = 0) then begin
  panic('Cannot Compute Geometric Mean for these numbers.');
end;
  compute_geometric_mean_mean := nth_root(abs(compute_geometric_mean_product), Length(nums));
  if compute_geometric_mean_product < 0 then begin
  compute_geometric_mean_mean := -compute_geometric_mean_mean;
end;
  compute_geometric_mean_possible := round_nearest(compute_geometric_mean_mean);
  if pow_int(compute_geometric_mean_possible, Length(nums)) = compute_geometric_mean_product then begin
  compute_geometric_mean_mean := compute_geometric_mean_possible;
end;
  exit(compute_geometric_mean_mean);
end;
procedure test_compute_geometric_mean();
var
  test_compute_geometric_mean_eps: real;
  test_compute_geometric_mean_m1: real;
  test_compute_geometric_mean_m2: real;
  test_compute_geometric_mean_m3: real;
  test_compute_geometric_mean_m4: real;
  test_compute_geometric_mean_m5: real;
begin
  test_compute_geometric_mean_eps := 0.0001;
  test_compute_geometric_mean_m1 := compute_geometric_mean([2, 8]);
  if abs(test_compute_geometric_mean_m1 - 4) > test_compute_geometric_mean_eps then begin
  panic('test1 failed');
end;
  test_compute_geometric_mean_m2 := compute_geometric_mean([5, 125]);
  if abs(test_compute_geometric_mean_m2 - 25) > test_compute_geometric_mean_eps then begin
  panic('test2 failed');
end;
  test_compute_geometric_mean_m3 := compute_geometric_mean([1, 0]);
  if abs(test_compute_geometric_mean_m3 - 0) > test_compute_geometric_mean_eps then begin
  panic('test3 failed');
end;
  test_compute_geometric_mean_m4 := compute_geometric_mean([1, 5, 25, 5]);
  if abs(test_compute_geometric_mean_m4 - 5) > test_compute_geometric_mean_eps then begin
  panic('test4 failed');
end;
  test_compute_geometric_mean_m5 := compute_geometric_mean([-5, 25, 1]);
  if abs(test_compute_geometric_mean_m5 + 5) > test_compute_geometric_mean_eps then begin
  panic('test5 failed');
end;
end;
procedure main();
begin
  test_compute_geometric_mean();
  writeln(compute_geometric_mean([-3, -27]));
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
