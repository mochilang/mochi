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
function int_pow(base: integer; exp: integer): integer; forward;
function backtrack(target: integer; exp: integer; current: integer; current_sum: integer): integer; forward;
function solve(target: integer; exp: integer): integer; forward;
function int_pow(base: integer; exp: integer): integer;
var
  int_pow_result_: integer;
  int_pow_i: integer;
begin
  int_pow_result_ := 1;
  int_pow_i := 0;
  while int_pow_i < exp do begin
  int_pow_result_ := int_pow_result_ * base;
  int_pow_i := int_pow_i + 1;
end;
  exit(int_pow_result_);
end;
function backtrack(target: integer; exp: integer; current: integer; current_sum: integer): integer;
var
  backtrack_p: integer;
  backtrack_count: integer;
begin
  if current_sum = target then begin
  exit(1);
end;
  backtrack_p := int_pow(current, exp);
  backtrack_count := 0;
  if (current_sum + backtrack_p) <= target then begin
  backtrack_count := backtrack_count + backtrack(target, exp, current + 1, current_sum + backtrack_p);
end;
  if backtrack_p < target then begin
  backtrack_count := backtrack_count + backtrack(target, exp, current + 1, current_sum);
end;
  exit(backtrack_count);
end;
function solve(target: integer; exp: integer): integer;
begin
  if not ((((1 <= target) and (target <= 1000)) and (2 <= exp)) and (exp <= 10)) then begin
  writeln('Invalid input');
  exit(0);
end;
  exit(backtrack(target, exp, 1, 0));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(solve(13, 2));
  writeln(solve(10, 2));
  writeln(solve(10, 3));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
