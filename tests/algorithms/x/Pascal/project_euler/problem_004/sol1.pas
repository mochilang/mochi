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
function solution(solution_n: int64): int64; forward;
function is_palindrome(is_palindrome_num: int64): boolean;
var
  is_palindrome_s: string;
  is_palindrome_i: int64;
  is_palindrome_j: integer;
begin
  is_palindrome_s := IntToStr(is_palindrome_num);
  is_palindrome_i := 0;
  is_palindrome_j := Length(is_palindrome_s) - 1;
  while is_palindrome_i < is_palindrome_j do begin
  if copy(is_palindrome_s, is_palindrome_i+1, (is_palindrome_i + 1 - (is_palindrome_i))) <> copy(is_palindrome_s, is_palindrome_j+1, (is_palindrome_j + 1 - (is_palindrome_j))) then begin
  exit(false);
end;
  is_palindrome_i := is_palindrome_i + 1;
  is_palindrome_j := is_palindrome_j - 1;
end;
  exit(true);
end;
function solution(solution_n: int64): int64;
var
  solution_number: int64;
  solution_divisor: int64;
  solution_other: int64;
begin
  solution_number := solution_n - 1;
  while solution_number > 9999 do begin
  if is_palindrome(solution_number) then begin
  solution_divisor := 999;
  while solution_divisor > 99 do begin
  if (solution_number mod solution_divisor) = 0 then begin
  solution_other := solution_number div solution_divisor;
  if Length(IntToStr(solution_other)) = 3 then begin
  exit(solution_number);
end;
end;
  solution_divisor := solution_divisor - 1;
end;
end;
  solution_number := solution_number - 1;
end;
  writeln('That number is larger than our acceptable range.');
  exit(0);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln('solution() = ' + IntToStr(solution(998001)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
