{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type PollardResult = record
  factor: integer;
  ok: boolean;
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
procedure error(msg: string);
begin
  panic(msg);
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  a: integer;
  attempts: integer;
  b: integer;
  modulus: integer;
  num: integer;
  seed: integer;
  step: integer;
  value: integer;
function makePollardResult(factor: integer; ok: boolean): PollardResult; forward;
function gcd(a: integer; b: integer): integer; forward;
function rand_fn(value: integer; step: integer; modulus: integer): integer; forward;
function pollard_rho(num: integer; seed: integer; step: integer; attempts: integer): PollardResult; forward;
procedure test_pollard_rho(); forward;
procedure main(); forward;
function makePollardResult(factor: integer; ok: boolean): PollardResult;
begin
  Result.factor := factor;
  Result.ok := ok;
end;
function gcd(a: integer; b: integer): integer;
var
  gcd_x: integer;
  gcd_y: integer;
  gcd_t: integer;
begin
  gcd_x := IfThen(a < 0, -a, a);
  gcd_y := IfThen(b < 0, -b, b);
  while gcd_y <> 0 do begin
  gcd_t := gcd_x mod gcd_y;
  gcd_x := gcd_y;
  gcd_y := gcd_t;
end;
  exit(gcd_x);
end;
function rand_fn(value: integer; step: integer; modulus: integer): integer;
begin
  exit(((value * value) + step) mod modulus);
end;
function pollard_rho(num: integer; seed: integer; step: integer; attempts: integer): PollardResult;
var
  pollard_rho_s: integer;
  pollard_rho_st: integer;
  pollard_rho_i: integer;
  pollard_rho_tortoise: integer;
  pollard_rho_hare: integer;
  pollard_rho_divisor: integer;
begin
  if num < 2 then begin
  panic('The input value cannot be less than 2');
end;
  if (num > 2) and ((num mod 2) = 0) then begin
  exit(makePollardResult(2, true));
end;
  pollard_rho_s := seed;
  pollard_rho_st := step;
  pollard_rho_i := 0;
  while pollard_rho_i < attempts do begin
  pollard_rho_tortoise := pollard_rho_s;
  pollard_rho_hare := pollard_rho_s;
  while true do begin
  pollard_rho_tortoise := rand_fn(pollard_rho_tortoise, pollard_rho_st, num);
  pollard_rho_hare := rand_fn(pollard_rho_hare, pollard_rho_st, num);
  pollard_rho_hare := rand_fn(pollard_rho_hare, pollard_rho_st, num);
  pollard_rho_divisor := gcd(pollard_rho_hare - pollard_rho_tortoise, num);
  if pollard_rho_divisor = 1 then begin
  continue;
end else begin
  if pollard_rho_divisor = num then begin
  break;
end else begin
  exit(makePollardResult(pollard_rho_divisor, true));
end;
end;
end;
  pollard_rho_s := pollard_rho_hare;
  pollard_rho_st := pollard_rho_st + 1;
  pollard_rho_i := pollard_rho_i + 1;
end;
  exit(makePollardResult(0, false));
end;
procedure test_pollard_rho();
var
  test_pollard_rho_r1: PollardResult;
  test_pollard_rho_r2: PollardResult;
  test_pollard_rho_r3: PollardResult;
  test_pollard_rho_r4: PollardResult;
  test_pollard_rho_r5: PollardResult;
  test_pollard_rho_r6: PollardResult;
  test_pollard_rho_r7: PollardResult;
begin
  test_pollard_rho_r1 := pollard_rho(8051, 2, 1, 5);
  if not test_pollard_rho_r1.ok or ((test_pollard_rho_r1.factor <> 83) and (test_pollard_rho_r1.factor <> 97)) then begin
  panic('test1 failed');
end;
  test_pollard_rho_r2 := pollard_rho(10403, 2, 1, 5);
  if not test_pollard_rho_r2.ok or ((test_pollard_rho_r2.factor <> 101) and (test_pollard_rho_r2.factor <> 103)) then begin
  panic('test2 failed');
end;
  test_pollard_rho_r3 := pollard_rho(100, 2, 1, 3);
  if not test_pollard_rho_r3.ok or (test_pollard_rho_r3.factor <> 2) then begin
  panic('test3 failed');
end;
  test_pollard_rho_r4 := pollard_rho(17, 2, 1, 3);
  if test_pollard_rho_r4.ok then begin
  panic('test4 failed');
end;
  test_pollard_rho_r5 := pollard_rho((17 * 17) * 17, 2, 1, 3);
  if not test_pollard_rho_r5.ok or (test_pollard_rho_r5.factor <> 17) then begin
  panic('test5 failed');
end;
  test_pollard_rho_r6 := pollard_rho((17 * 17) * 17, 2, 1, 1);
  if test_pollard_rho_r6.ok then begin
  panic('test6 failed');
end;
  test_pollard_rho_r7 := pollard_rho((3 * 5) * 7, 2, 1, 3);
  if not test_pollard_rho_r7.ok or (test_pollard_rho_r7.factor <> 21) then begin
  panic('test7 failed');
end;
end;
procedure main();
var
  main_a: PollardResult;
  main_b: PollardResult;
begin
  test_pollard_rho();
  main_a := pollard_rho(100, 2, 1, 3);
  if main_a.ok then begin
  writeln(IntToStr(main_a.factor));
end else begin
  writeln('None');
end;
  main_b := pollard_rho(17, 2, 1, 3);
  if main_b.ok then begin
  writeln(IntToStr(main_b.factor));
end else begin
  writeln('None');
end;
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
