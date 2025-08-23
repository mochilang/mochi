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
function is_palindrome(is_palindrome_num: int64): boolean; forward;
function solution(solution_limit: int64): int64; forward;
function is_palindrome(is_palindrome_num: int64): boolean;
var
  is_palindrome_n: int64;
  is_palindrome_rev: int64;
begin
  if is_palindrome_num < 0 then begin
  exit(false);
end;
  is_palindrome_n := is_palindrome_num;
  is_palindrome_rev := 0;
  while is_palindrome_n > 0 do begin
  is_palindrome_rev := (is_palindrome_rev * 10) + (is_palindrome_n mod 10);
  is_palindrome_n := is_palindrome_n div 10;
end;
  exit(is_palindrome_rev = is_palindrome_num);
end;
function solution(solution_limit: int64): int64;
var
  solution_answer: int64;
  solution_i: int64;
  solution_j: int64;
  solution_product: int64;
begin
  solution_answer := 0;
  solution_i := 999;
  while solution_i >= 100 do begin
  solution_j := 999;
  while solution_j >= 100 do begin
  solution_product := solution_i * solution_j;
  if ((solution_product < solution_limit) and is_palindrome(solution_product)) and (solution_product > solution_answer) then begin
  solution_answer := solution_product;
end;
  solution_j := solution_j - 1;
end;
  solution_i := solution_i - 1;
end;
  exit(solution_answer);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(solution(998001)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
