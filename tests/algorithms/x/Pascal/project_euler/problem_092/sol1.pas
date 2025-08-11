{$mode objfpc}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  number: integer;
  limit: integer;
function next_number(number: integer): integer; forward;
function chain(number: integer): boolean; forward;
function solution(limit: integer): integer; forward;
function next_number(number: integer): integer;
var
  next_number_n: integer;
  next_number_total: integer;
  next_number_d: integer;
begin
  next_number_n := number;
  next_number_total := 0;
  while next_number_n > 0 do begin
  next_number_d := next_number_n mod 10;
  next_number_total := next_number_total + (next_number_d * next_number_d);
  next_number_n := next_number_n div 10;
end;
  exit(next_number_total);
end;
function chain(number: integer): boolean;
var
  chain_n: integer;
begin
  chain_n := number;
  while (chain_n <> 1) and (chain_n <> 89) do begin
  chain_n := next_number(chain_n);
end;
  exit(chain_n = 1);
end;
function solution(limit: integer): integer;
var
  solution_count: integer;
  solution_i: integer;
begin
  solution_count := 0;
  solution_i := 1;
  while solution_i < limit do begin
  if not chain(solution_i) then begin
  solution_count := solution_count + 1;
end;
  solution_i := solution_i + 1;
end;
  exit(solution_count);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(next_number(44)));
  writeln(IntToStr(next_number(10)));
  writeln(IntToStr(next_number(32)));
  writeln(LowerCase(BoolToStr(chain(10), true)));
  writeln(LowerCase(BoolToStr(chain(58), true)));
  writeln(LowerCase(BoolToStr(chain(1), true)));
  writeln(IntToStr(solution(100)));
  writeln(IntToStr(solution(1000)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
