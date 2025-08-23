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
function gcd(gcd_a: int64; gcd_b: int64): int64; forward;
function lcm(lcm_a: int64; lcm_b: int64): int64; forward;
function solution(solution_n: int64): int64; forward;
function gcd(gcd_a: int64; gcd_b: int64): int64;
var
  gcd_x: int64;
  gcd_y: int64;
  gcd_t: int64;
begin
  gcd_x := gcd_a;
  gcd_y := gcd_b;
  while gcd_y <> 0 do begin
  gcd_t := gcd_x mod gcd_y;
  gcd_x := gcd_y;
  gcd_y := gcd_t;
end;
  if gcd_x < 0 then begin
  exit(-gcd_x);
end;
  exit(gcd_x);
end;
function lcm(lcm_a: int64; lcm_b: int64): int64;
begin
  exit((lcm_a div gcd(lcm_a, lcm_b)) * lcm_b);
end;
function solution(solution_n: int64): int64;
var
  solution_result_: int64;
  solution_i: int64;
begin
  if solution_n <= 0 then begin
  panic('Parameter n must be greater than or equal to one.');
end;
  solution_result_ := 1;
  solution_i := 2;
  while solution_i <= solution_n do begin
  solution_result_ := lcm(solution_result_, solution_i);
  solution_i := solution_i + 1;
end;
  exit(solution_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(solution(10));
  writeln(solution(15));
  writeln(solution(22));
  writeln(solution(20));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
