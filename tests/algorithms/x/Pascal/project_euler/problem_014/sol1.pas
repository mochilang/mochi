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
function _input(): string;
var s: string;
begin
  if EOF(Input) then s := '' else ReadLn(s);
  _input := s;
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
  input_str: string;
  n: int64;
function solution(solution_n: int64): int64; forward;
function solution(solution_n: int64): int64;
var
  solution_counters: array of int64;
  solution_i: int64;
  solution_largest_number: int64;
  solution_pre_counter: int64;
  solution_start: int64;
  solution_number: int64;
  solution_counter: int64;
begin
  solution_i := 0;
  while solution_i <= solution_n do begin
  solution_counters := concat(solution_counters, IntArray([0]));
  solution_i := solution_i + 1;
end;
  solution_counters[1] := 1;
  solution_largest_number := 1;
  solution_pre_counter := 1;
  solution_start := 2;
  while solution_start < solution_n do begin
  solution_number := solution_start;
  solution_counter := 0;
  while true do begin
  if (solution_number < Length(solution_counters)) and (solution_counters[solution_number] <> 0) then begin
  solution_counter := solution_counter + solution_counters[solution_number];
  break;
end;
  if (solution_number mod 2) = 0 then begin
  solution_number := solution_number div 2;
end else begin
  solution_number := (3 * solution_number) + 1;
end;
  solution_counter := solution_counter + 1;
end;
  if (solution_start < Length(solution_counters)) and (solution_counters[solution_start] = 0) then begin
  solution_counters[solution_start] := solution_counter;
end;
  if solution_counter > solution_pre_counter then begin
  solution_largest_number := solution_start;
  solution_pre_counter := solution_counter;
end;
  solution_start := solution_start + 1;
end;
  exit(solution_largest_number);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  input_str := _input();
  n := StrToInt(input_str);
  writeln(IntToStr(solution(n)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
