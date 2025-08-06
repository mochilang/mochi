{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  maze: array of array of integer;
  n: integer;
function run_maze(maze: IntArrayArray; i: integer; j: integer; dr: integer; dc: integer; sol: IntArrayArray): boolean; forward;
function solve_maze(maze: IntArrayArray; sr: integer; sc: integer; dr: integer; dc: integer): IntArrayArray; forward;
function run_maze(maze: IntArrayArray; i: integer; j: integer; dr: integer; dc: integer; sol: IntArrayArray): boolean;
var
  run_maze_size: integer;
  run_maze_lower_flag: boolean;
  run_maze_upper_flag: boolean;
  run_maze_block_flag: boolean;
begin
  run_maze_size := Length(maze);
  if ((i = dr) and (j = dc)) and (maze[i][j] = 0) then begin
  sol[i][j] := 0;
  exit(true);
end;
  run_maze_lower_flag := (i >= 0) and (j >= 0);
  run_maze_upper_flag := (i < run_maze_size) and (j < run_maze_size);
  if run_maze_lower_flag and run_maze_upper_flag then begin
  run_maze_block_flag := (sol[i][j] = 1) and (maze[i][j] = 0);
  if run_maze_block_flag then begin
  sol[i][j] := 0;
  if ((run_maze(maze, i + 1, j, dr, dc, sol) or run_maze(maze, i, j + 1, dr, dc, sol)) or run_maze(maze, i - 1, j, dr, dc, sol)) or run_maze(maze, i, j - 1, dr, dc, sol) then begin
  exit(true);
end;
  sol[i][j] := 1;
  exit(false);
end;
end;
  exit(false);
end;
function solve_maze(maze: IntArrayArray; sr: integer; sc: integer; dr: integer; dc: integer): IntArrayArray;
var
  solve_maze_size: integer;
  solve_maze_sol: array of IntArray;
  solve_maze_i: integer;
  solve_maze_row: array of integer;
  solve_maze_j: integer;
  solve_maze_solved: boolean;
begin
  solve_maze_size := Length(maze);
  if not ((((((((0 <= sr) and (sr < solve_maze_size)) and (0 <= sc)) and (sc < solve_maze_size)) and (0 <= dr)) and (dr < solve_maze_size)) and (0 <= dc)) and (dc < solve_maze_size)) then begin
  panic('Invalid source or destination coordinates');
end;
  solve_maze_sol := [];
  solve_maze_i := 0;
  while solve_maze_i < solve_maze_size do begin
  solve_maze_row := [];
  solve_maze_j := 0;
  while solve_maze_j < solve_maze_size do begin
  solve_maze_row := concat(solve_maze_row, [1]);
  solve_maze_j := solve_maze_j + 1;
end;
  solve_maze_sol := concat(solve_maze_sol, [solve_maze_row]);
  solve_maze_i := solve_maze_i + 1;
end;
  solve_maze_solved := run_maze(maze, sr, sc, dr, dc, solve_maze_sol);
  if solve_maze_solved then begin
  exit(solve_maze_sol);
end else begin
  panic('No solution exists!');
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  maze := [[0, 1, 0, 1, 1], [0, 0, 0, 0, 0], [1, 0, 1, 0, 1], [0, 0, 1, 0, 0], [1, 0, 0, 1, 0]];
  n := Length(maze) - 1;
  writeln(list_list_int_to_str(solve_maze(maze, 0, 0, n, n)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
