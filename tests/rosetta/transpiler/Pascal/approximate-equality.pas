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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function abs(x: real): real; forward;
function maxf(a: real; b: real): real; forward;
function isClose(a: real; b: real): boolean; forward;
function sqrtApprox(x: real): real; forward;
procedure main(); forward;
function abs(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function maxf(a: real; b: real): real;
begin
  if a > b then begin
  exit(a);
end;
  exit(b);
end;
function isClose(a: real; b: real): boolean;
var
  isClose_relTol: real;
  isClose_t: real;
  isClose_u: real;
begin
  isClose_relTol := 1e-09;
  isClose_t := abs(a - b);
  isClose_u := isClose_relTol * maxf(abs(a), abs(b));
  exit(isClose_t <= isClose_u);
end;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  sqrtApprox_guess := x;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 10 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
procedure main();
var
  main_root2: real;
  main_pairs: array of array of real;
  main_pair: integer;
  main_a: integer;
  main_b: integer;
  main_s: string;
begin
  main_root2 := sqrtApprox(2);
  main_pairs := [[1.0000000000000002e+14, 1.0000000000000002e+14], [100.01, 100.011], [1.0000000000000002e+13 / 10000, 1.0000000000000001e+09], [0.001, 0.0010000001], [1.01e-22, 0], [main_root2 * main_root2, 2], [-main_root2 * main_root2, -2], [1e+17, 1e+17], [3.141592653589793, 3.141592653589793]];
  for main_pair in main_pairs do begin
  main_a := main_pair[0];
  main_b := main_pair[1];
  main_s := IfThen(isClose(main_a, main_b), '≈', '≉');
  writeln((((IntToStr(main_a) + ' ') + main_s) + ' ') + IntToStr(main_b));
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
