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
function power_of_two(power_of_two_exp_: int64): int64; forward;
function solution(solution_power: int64): int64; forward;
function power_of_two(power_of_two_exp_: int64): int64;
var
  power_of_two_result_: int64;
  power_of_two_i: int64;
begin
  power_of_two_result_ := 1;
  power_of_two_i := 0;
  while power_of_two_i < power_of_two_exp_ do begin
  power_of_two_result_ := power_of_two_result_ * 2;
  power_of_two_i := power_of_two_i + 1;
end;
  exit(power_of_two_result_);
end;
function solution(solution_power: int64): int64;
var
  solution_num: int64;
  solution_string_num: string;
  solution_sum: int64;
  solution_i: int64;
begin
  solution_num := power_of_two(solution_power);
  solution_string_num := IntToStr(solution_num);
  solution_sum := 0;
  solution_i := 0;
  while solution_i < Length(solution_string_num) do begin
  solution_sum := solution_sum + StrToInt(solution_string_num[solution_i+1]);
  solution_i := solution_i + 1;
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
