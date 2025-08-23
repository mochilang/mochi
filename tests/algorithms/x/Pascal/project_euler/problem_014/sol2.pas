{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
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
  collatz_cache: specialize TFPGMap<int64, int64>;
  input_str: string;
  limit: int64;
function Map1(): specialize TFPGMap<int64, int64>; forward;
function collatz_length(collatz_length_n: int64): int64; forward;
function solution(solution_limit: int64): int64; forward;
function Map1(): specialize TFPGMap<int64, int64>;
begin
  Result := specialize TFPGMap<int64, int64>.Create();
  Result.AddOrSetData(1, 1);
end;
function collatz_length(collatz_length_n: int64): int64;
var
  collatz_length_num: int64;
  collatz_length_sequence: array of int64;
  collatz_length_length_: int64;
  collatz_length_i: integer;
begin
  collatz_length_num := collatz_length_n;
  collatz_length_sequence := [];
  while not collatz_cache.IndexOf(collatz_length_num) <> -1 do begin
  collatz_length_sequence := concat(collatz_length_sequence, IntArray([collatz_length_num]));
  if (collatz_length_num mod 2) = 0 then begin
  collatz_length_num := Trunc(collatz_length_num div 2);
end else begin
  collatz_length_num := (3 * collatz_length_num) + 1;
end;
end;
  collatz_length_length_ := collatz_cache[collatz_length_num];
  collatz_length_i := Length(collatz_length_sequence) - 1;
  while collatz_length_i >= 0 do begin
  collatz_length_length_ := collatz_length_length_ + 1;
  collatz_cache[collatz_length_sequence[collatz_length_i]] := collatz_length_length_;
  collatz_length_i := collatz_length_i - 1;
end;
  exit(collatz_length_length_);
end;
function solution(solution_limit: int64): int64;
var
  solution_max_len: int64;
  solution_max_start: int64;
  solution_i: int64;
  solution_length_: int64;
begin
  solution_max_len := 0;
  solution_max_start := 1;
  solution_i := 1;
  while solution_i < solution_limit do begin
  solution_length_ := collatz_length(solution_i);
  if solution_length_ > solution_max_len then begin
  solution_max_len := solution_length_;
  solution_max_start := solution_i;
end;
  solution_i := solution_i + 1;
end;
  exit(solution_max_start);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  collatz_cache := Map1();
  input_str := _input();
  limit := StrToInt(input_str);
  writeln(solution(limit));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
