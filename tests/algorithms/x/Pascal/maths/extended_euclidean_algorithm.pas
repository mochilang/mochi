{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Coeffs = record
  x: integer;
  y: integer;
end;
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
  b: integer;
  a: integer;
  n: integer;
function makeCoeffs(x: integer; y: integer): Coeffs; forward;
function abs_val(n: integer): integer; forward;
function extended_euclidean_algorithm(a: integer; b: integer): Coeffs; forward;
procedure test_extended_euclidean_algorithm(); forward;
procedure main(); forward;
function makeCoeffs(x: integer; y: integer): Coeffs;
begin
  Result.x := x;
  Result.y := y;
end;
function abs_val(n: integer): integer;
begin
  if n < 0 then begin
  exit(-n);
end;
  exit(n);
end;
function extended_euclidean_algorithm(a: integer; b: integer): Coeffs;
var
  extended_euclidean_algorithm_old_remainder: integer;
  extended_euclidean_algorithm_remainder: integer;
  extended_euclidean_algorithm_old_coeff_a: integer;
  extended_euclidean_algorithm_coeff_a: integer;
  extended_euclidean_algorithm_old_coeff_b: integer;
  extended_euclidean_algorithm_coeff_b: integer;
  extended_euclidean_algorithm_quotient: integer;
  extended_euclidean_algorithm_temp_remainder: integer;
  extended_euclidean_algorithm_temp_a: integer;
  extended_euclidean_algorithm_temp_b: integer;
begin
  if abs_val(a) = 1 then begin
  exit(makeCoeffs(a, 0));
end;
  if abs_val(b) = 1 then begin
  exit(makeCoeffs(0, b));
end;
  extended_euclidean_algorithm_old_remainder := a;
  extended_euclidean_algorithm_remainder := b;
  extended_euclidean_algorithm_old_coeff_a := 1;
  extended_euclidean_algorithm_coeff_a := 0;
  extended_euclidean_algorithm_old_coeff_b := 0;
  extended_euclidean_algorithm_coeff_b := 1;
  while extended_euclidean_algorithm_remainder <> 0 do begin
  extended_euclidean_algorithm_quotient := extended_euclidean_algorithm_old_remainder div extended_euclidean_algorithm_remainder;
  extended_euclidean_algorithm_temp_remainder := extended_euclidean_algorithm_old_remainder - (extended_euclidean_algorithm_quotient * extended_euclidean_algorithm_remainder);
  extended_euclidean_algorithm_old_remainder := extended_euclidean_algorithm_remainder;
  extended_euclidean_algorithm_remainder := extended_euclidean_algorithm_temp_remainder;
  extended_euclidean_algorithm_temp_a := extended_euclidean_algorithm_old_coeff_a - (extended_euclidean_algorithm_quotient * extended_euclidean_algorithm_coeff_a);
  extended_euclidean_algorithm_old_coeff_a := extended_euclidean_algorithm_coeff_a;
  extended_euclidean_algorithm_coeff_a := extended_euclidean_algorithm_temp_a;
  extended_euclidean_algorithm_temp_b := extended_euclidean_algorithm_old_coeff_b - (extended_euclidean_algorithm_quotient * extended_euclidean_algorithm_coeff_b);
  extended_euclidean_algorithm_old_coeff_b := extended_euclidean_algorithm_coeff_b;
  extended_euclidean_algorithm_coeff_b := extended_euclidean_algorithm_temp_b;
end;
  if a < 0 then begin
  extended_euclidean_algorithm_old_coeff_a := -extended_euclidean_algorithm_old_coeff_a;
end;
  if b < 0 then begin
  extended_euclidean_algorithm_old_coeff_b := -extended_euclidean_algorithm_old_coeff_b;
end;
  exit(makeCoeffs(extended_euclidean_algorithm_old_coeff_a, extended_euclidean_algorithm_old_coeff_b));
end;
procedure test_extended_euclidean_algorithm();
var
  test_extended_euclidean_algorithm_r1: Coeffs;
  test_extended_euclidean_algorithm_r2: Coeffs;
  test_extended_euclidean_algorithm_r3: Coeffs;
  test_extended_euclidean_algorithm_r4: Coeffs;
  test_extended_euclidean_algorithm_r5: Coeffs;
  test_extended_euclidean_algorithm_r6: Coeffs;
  test_extended_euclidean_algorithm_r7: Coeffs;
begin
  test_extended_euclidean_algorithm_r1 := extended_euclidean_algorithm(1, 24);
  if (test_extended_euclidean_algorithm_r1.x <> 1) or (test_extended_euclidean_algorithm_r1.y <> 0) then begin
  panic('test1 failed');
end;
  test_extended_euclidean_algorithm_r2 := extended_euclidean_algorithm(8, 14);
  if (test_extended_euclidean_algorithm_r2.x <> 2) or (test_extended_euclidean_algorithm_r2.y <> -1) then begin
  panic('test2 failed');
end;
  test_extended_euclidean_algorithm_r3 := extended_euclidean_algorithm(240, 46);
  if (test_extended_euclidean_algorithm_r3.x <> -9) or (test_extended_euclidean_algorithm_r3.y <> 47) then begin
  panic('test3 failed');
end;
  test_extended_euclidean_algorithm_r4 := extended_euclidean_algorithm(1, -4);
  if (test_extended_euclidean_algorithm_r4.x <> 1) or (test_extended_euclidean_algorithm_r4.y <> 0) then begin
  panic('test4 failed');
end;
  test_extended_euclidean_algorithm_r5 := extended_euclidean_algorithm(-2, -4);
  if (test_extended_euclidean_algorithm_r5.x <> -1) or (test_extended_euclidean_algorithm_r5.y <> 0) then begin
  panic('test5 failed');
end;
  test_extended_euclidean_algorithm_r6 := extended_euclidean_algorithm(0, -4);
  if (test_extended_euclidean_algorithm_r6.x <> 0) or (test_extended_euclidean_algorithm_r6.y <> -1) then begin
  panic('test6 failed');
end;
  test_extended_euclidean_algorithm_r7 := extended_euclidean_algorithm(2, 0);
  if (test_extended_euclidean_algorithm_r7.x <> 1) or (test_extended_euclidean_algorithm_r7.y <> 0) then begin
  panic('test7 failed');
end;
end;
procedure main();
var
  main_res: Coeffs;
begin
  test_extended_euclidean_algorithm();
  main_res := extended_euclidean_algorithm(240, 46);
  writeln(((('(' + IntToStr(main_res.x)) + ', ') + IntToStr(main_res.y)) + ')');
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
