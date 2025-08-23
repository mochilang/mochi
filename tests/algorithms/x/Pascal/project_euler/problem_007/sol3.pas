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
function is_prime(is_prime_number: int64): boolean; forward;
function solution(solution_nth: int64): int64; forward;
function is_prime(is_prime_number: int64): boolean;
var
  is_prime_i: int64;
begin
  if (1 < is_prime_number) and (is_prime_number < 4) then begin
  exit(true);
end else begin
  if ((is_prime_number < 2) or ((is_prime_number mod 2) = 0)) or ((is_prime_number mod 3) = 0) then begin
  exit(false);
end;
end;
  is_prime_i := 5;
  while (is_prime_i * is_prime_i) <= is_prime_number do begin
  if ((is_prime_number mod is_prime_i) = 0) or ((is_prime_number mod (is_prime_i + 2)) = 0) then begin
  exit(false);
end;
  is_prime_i := is_prime_i + 6;
end;
  exit(true);
end;
function solution(solution_nth: int64): int64;
var
  solution_count: int64;
  solution_num: int64;
begin
  solution_count := 0;
  solution_num := 2;
  while true do begin
  if is_prime(solution_num) then begin
  solution_count := solution_count + 1;
  if solution_count = solution_nth then begin
  exit(solution_num);
end;
end;
  solution_num := solution_num + 1;
end;
  exit(0);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln('solution() = ' + IntToStr(solution(10001)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
