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
function solution(solution_n: int64): int64; forward;
procedure main(); forward;
function is_prime(is_prime_number: int64): boolean;
var
  is_prime_i: int64;
begin
  if (is_prime_number > 1) and (is_prime_number < 4) then begin
  exit(true);
end;
  if ((is_prime_number < 2) or ((is_prime_number mod 2) = 0)) or ((is_prime_number mod 3) = 0) then begin
  exit(false);
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
function solution(solution_n: int64): int64;
var
  solution_num: int64;
  solution_max_number: int64;
  solution_i: int64;
begin
  solution_num := solution_n;
  if solution_num <= 0 then begin
  writeln('Parameter n must be greater than or equal to one.');
  exit(0);
end;
  if is_prime(solution_num) then begin
  exit(solution_num);
end;
  while (solution_num mod 2) = 0 do begin
  solution_num := solution_num div 2;
  if is_prime(solution_num) then begin
  exit(solution_num);
end;
end;
  solution_max_number := 1;
  solution_i := 3;
  while (solution_i * solution_i) <= solution_num do begin
  if (solution_num mod solution_i) = 0 then begin
  if is_prime(solution_num div solution_i) then begin
  solution_max_number := solution_num div solution_i;
  break;
end else begin
  if is_prime(solution_i) then begin
  solution_max_number := solution_i;
end;
end;
end;
  solution_i := solution_i + 2;
end;
  exit(solution_max_number);
end;
procedure main();
var
  main_result_: int64;
begin
  main_result_ := solution(600851475143);
  writeln('solution() = ' + IntToStr(main_result_));
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
