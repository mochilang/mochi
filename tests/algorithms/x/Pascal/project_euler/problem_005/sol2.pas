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
function lcm(lcm_x: int64; lcm_y: int64): int64; forward;
function solution(solution_n: int64): int64; forward;
procedure main(); forward;
function gcd(gcd_a: int64; gcd_b: int64): int64;
var
  gcd_x: int64;
  gcd_y: int64;
  gcd_t: int64;
begin
  gcd_x := gcd_a;
  gcd_y := gcd_b;
  while gcd_y <> 0 do begin
  gcd_t := gcd_y;
  gcd_y := gcd_x mod gcd_y;
  gcd_x := gcd_t;
end;
  exit(gcd_x);
end;
function lcm(lcm_x: int64; lcm_y: int64): int64;
begin
  exit((lcm_x * lcm_y) div gcd(lcm_x, lcm_y));
end;
function solution(solution_n: int64): int64;
var
  solution_g: int64;
  solution_i: int64;
begin
  solution_g := 1;
  solution_i := 1;
  while solution_i <= solution_n do begin
  solution_g := lcm(solution_g, solution_i);
  solution_i := solution_i + 1;
end;
  exit(solution_g);
end;
procedure main();
begin
  writeln(IntToStr(solution(20)));
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
  writeln('');
end.
