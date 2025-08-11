{$mode objfpc}
program Main;
uses SysUtils;
type SearchProblem = record
  x: real;
  y: real;
  step: real;
  f: FuncType1;
end;
type FuncType1 = function(arg0: real; arg1: real): real;
type SearchProblemArray = array of SearchProblem;
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
  min_y: real;
  min_x: real;
  sp: SearchProblem;
  lst: SearchProblemArray;
  y: real;
  max_y: real;
  x: real;
  find_max: boolean;
  max_iter: integer;
  a: SearchProblem;
  b: SearchProblem;
  max_x: real;
function makeSearchProblem(x: real; y: real; step: real; f: FuncType1): SearchProblem; forward;
function score(sp: SearchProblem): real; forward;
function neighbors(sp: SearchProblem): SearchProblemArray; forward;
function equal_state(a: SearchProblem; b: SearchProblem): boolean; forward;
function contains_state(lst: SearchProblemArray; sp: SearchProblem): boolean; forward;
function hill_climbing(sp: SearchProblem; find_max: boolean; max_x: real; min_x: real; max_y: real; min_y: real; max_iter: integer): SearchProblem; forward;
function test_f1(x: real; y: real): real; forward;
procedure main(); forward;
function makeSearchProblem(x: real; y: real; step: real; f: FuncType1): SearchProblem;
begin
  Result.x := x;
  Result.y := y;
  Result.step := step;
  Result.f := f;
end;
function score(sp: SearchProblem): real;
begin
  exit(sp.f);
end;
function neighbors(sp: SearchProblem): SearchProblemArray;
var
  neighbors_s: real;
begin
  neighbors_s := sp.step;
  exit([makeSearchProblem(sp.x - neighbors_s, sp.y - neighbors_s, neighbors_s, sp.f), makeSearchProblem(sp.x - neighbors_s, sp.y, neighbors_s, sp.f), makeSearchProblem(sp.x - neighbors_s, sp.y + neighbors_s, neighbors_s, sp.f), makeSearchProblem(sp.x, sp.y - neighbors_s, neighbors_s, sp.f), makeSearchProblem(sp.x, sp.y + neighbors_s, neighbors_s, sp.f), makeSearchProblem(sp.x + neighbors_s, sp.y - neighbors_s, neighbors_s, sp.f), makeSearchProblem(sp.x + neighbors_s, sp.y, neighbors_s, sp.f), makeSearchProblem(sp.x + neighbors_s, sp.y + neighbors_s, neighbors_s, sp.f)]);
end;
function equal_state(a: SearchProblem; b: SearchProblem): boolean;
begin
  exit((a.x = b.x) and (a.y = b.y));
end;
function contains_state(lst: SearchProblemArray; sp: SearchProblem): boolean;
var
  contains_state_i: integer;
begin
  contains_state_i := 0;
  while contains_state_i < Length(lst) do begin
  if equal_state(lst[contains_state_i], sp) then begin
  exit(true);
end;
  contains_state_i := contains_state_i + 1;
end;
  exit(false);
end;
function hill_climbing(sp: SearchProblem; find_max: boolean; max_x: real; min_x: real; max_y: real; min_y: real; max_iter: integer): SearchProblem;
var
  hill_climbing_current: SearchProblem;
  hill_climbing_visited: array of SearchProblem;
  hill_climbing_iterations: integer;
  hill_climbing_solution_found: boolean;
  hill_climbing_current_score: real;
  hill_climbing_neighs: SearchProblemArray;
  hill_climbing_max_change: real;
  hill_climbing_min_change: real;
  hill_climbing_next: SearchProblem;
  hill_climbing_improved: boolean;
  hill_climbing_i: integer;
  hill_climbing_n: SearchProblem;
  hill_climbing_change: real;
begin
  hill_climbing_current := sp;
  hill_climbing_visited := [];
  hill_climbing_iterations := 0;
  hill_climbing_solution_found := false;
  while (hill_climbing_solution_found = false) and (hill_climbing_iterations < max_iter) do begin
  hill_climbing_visited := concat(hill_climbing_visited, [hill_climbing_current]);
  hill_climbing_iterations := hill_climbing_iterations + 1;
  hill_climbing_current_score := score(hill_climbing_current);
  hill_climbing_neighs := neighbors(hill_climbing_current);
  hill_climbing_max_change := -1e+18;
  hill_climbing_min_change := 1e+18;
  hill_climbing_next := hill_climbing_current;
  hill_climbing_improved := false;
  hill_climbing_i := 0;
  while hill_climbing_i < Length(hill_climbing_neighs) do begin
  hill_climbing_n := hill_climbing_neighs[hill_climbing_i];
  hill_climbing_i := hill_climbing_i + 1;
  if contains_state(hill_climbing_visited, hill_climbing_n) then begin
  continue;
end;
  if (((hill_climbing_n.x > max_x) or (hill_climbing_n.x < min_x)) or (hill_climbing_n.y > max_y)) or (hill_climbing_n.y < min_y) then begin
  continue;
end;
  hill_climbing_change := score(hill_climbing_n) - hill_climbing_current_score;
  if find_max then begin
  if (hill_climbing_change > hill_climbing_max_change) and (hill_climbing_change > 0) then begin
  hill_climbing_max_change := hill_climbing_change;
  hill_climbing_next := hill_climbing_n;
  hill_climbing_improved := true;
end;
end else begin
  if (hill_climbing_change < hill_climbing_min_change) and (hill_climbing_change < 0) then begin
  hill_climbing_min_change := hill_climbing_change;
  hill_climbing_next := hill_climbing_n;
  hill_climbing_improved := true;
end;
end;
end;
  if hill_climbing_improved then begin
  hill_climbing_current := hill_climbing_next;
end else begin
  hill_climbing_solution_found := true;
end;
end;
  exit(hill_climbing_current);
end;
function test_f1(x: real; y: real): real;
begin
  exit((x * x) + (y * y));
end;
procedure main();
var
  main_prob1: SearchProblem;
  main_local_min1: SearchProblem;
  main_prob2: SearchProblem;
  main_local_min2: SearchProblem;
  main_prob3: SearchProblem;
  main_local_max: SearchProblem;
begin
  main_prob1 := makeSearchProblem(3, 4, 1, @test_f1);
  main_local_min1 := hill_climbing(main_prob1, false, 1e+09, -1e+09, 1e+09, -1e+09, 10000);
  writeln(IntToStr(Trunc(score(main_local_min1))));
  main_prob2 := makeSearchProblem(12, 47, 1, @test_f1);
  main_local_min2 := hill_climbing(main_prob2, false, 100, 5, 50, -5, 10000);
  writeln(IntToStr(Trunc(score(main_local_min2))));
  main_prob3 := makeSearchProblem(3, 4, 1, @test_f1);
  main_local_max := hill_climbing(main_prob3, true, 1e+09, -1e+09, 1e+09, -1e+09, 1000);
  writeln(IntToStr(Trunc(score(main_local_max))));
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
