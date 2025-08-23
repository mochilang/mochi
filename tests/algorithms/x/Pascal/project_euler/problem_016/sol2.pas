{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
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
function solution(solution_power: int64): int64; forward;
function solution(solution_power: int64): int64;
var
  solution_digits: array of int64;
  solution_i: int64;
  solution_carry: int64;
  solution_j: int64;
  solution_v: int64;
  solution_sum: int64;
  solution_k: int64;
begin
  solution_digits := [];
  solution_digits := concat(solution_digits, IntArray([1]));
  solution_i := 0;
  while solution_i < solution_power do begin
  solution_carry := 0;
  solution_j := 0;
  while solution_j < Length(solution_digits) do begin
  solution_v := (solution_digits[solution_j] * 2) + solution_carry;
  solution_digits[solution_j] := solution_v mod 10;
  solution_carry := solution_v div 10;
  solution_j := solution_j + 1;
end;
  if solution_carry > 0 then begin
  solution_digits := concat(solution_digits, IntArray([solution_carry]));
end;
  solution_i := solution_i + 1;
end;
  solution_sum := 0;
  solution_k := 0;
  while solution_k < Length(solution_digits) do begin
  solution_sum := solution_sum + solution_digits[solution_k];
  solution_k := solution_k + 1;
end;
  exit(solution_sum);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(solution(1000)));
  writeln(IntToStr(solution(50)));
  writeln(IntToStr(solution(20)));
  writeln(IntToStr(solution(15)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
