{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of integer;
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
  xs: IntArray;
  value: integer;
  n: integer;
function contains(xs: IntArray; value: integer): boolean; forward;
function solution(n: integer): integer; forward;
procedure test_solution(); forward;
procedure main(); forward;
function contains(xs: IntArray; value: integer): boolean;
var
  contains_i: integer;
begin
  contains_i := 0;
  while contains_i < Length(xs) do begin
  if xs[contains_i] = value then begin
  exit(true);
end;
  contains_i := contains_i + 1;
end;
  exit(false);
end;
function solution(n: integer): integer;
var
  solution_zmulti: array of integer;
  solution_xmulti: array of integer;
  solution_temp: integer;
  solution_result_: integer;
  solution_collection: array of integer;
  solution_i: integer;
  solution_v: integer;
  solution_total: integer;
begin
  solution_zmulti := [];
  solution_xmulti := [];
  solution_temp := 1;
  while true do begin
  solution_result_ := 3 * solution_temp;
  if solution_result_ < n then begin
  solution_zmulti := concat(solution_zmulti, IntArray([solution_result_]));
  solution_temp := solution_temp + 1;
end else begin
  break;
end;
end;
  solution_temp := 1;
  while true do begin
  solution_result_ := 5 * solution_temp;
  if solution_result_ < n then begin
  solution_xmulti := concat(solution_xmulti, IntArray([solution_result_]));
  solution_temp := solution_temp + 1;
end else begin
  break;
end;
end;
  solution_collection := [];
  solution_i := 0;
  while solution_i < Length(solution_zmulti) do begin
  solution_v := solution_zmulti[solution_i];
  if not contains(solution_collection, solution_v) then begin
  solution_collection := concat(solution_collection, IntArray([solution_v]));
end;
  solution_i := solution_i + 1;
end;
  solution_i := 0;
  while solution_i < Length(solution_xmulti) do begin
  solution_v := solution_xmulti[solution_i];
  if not contains(solution_collection, solution_v) then begin
  solution_collection := concat(solution_collection, IntArray([solution_v]));
end;
  solution_i := solution_i + 1;
end;
  solution_total := 0;
  solution_i := 0;
  while solution_i < Length(solution_collection) do begin
  solution_total := solution_total + solution_collection[solution_i];
  solution_i := solution_i + 1;
end;
  exit(solution_total);
end;
procedure test_solution();
begin
  if solution(3) <> 0 then begin
  panic('solution(3) failed');
end;
  if solution(4) <> 3 then begin
  panic('solution(4) failed');
end;
  if solution(10) <> 23 then begin
  panic('solution(10) failed');
end;
  if solution(600) <> 83700 then begin
  panic('solution(600) failed');
end;
end;
procedure main();
begin
  test_solution();
  writeln('solution() = ' + IntToStr(solution(1000)));
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
end.
