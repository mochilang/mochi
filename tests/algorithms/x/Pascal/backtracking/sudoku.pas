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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  initial_grid: array of array of integer;
  no_solution: array of array of integer;
  examples: array of array of array of integer;
  idx: integer;
function is_safe(grid: IntArrayArray; row: integer; column: integer; n: integer): boolean; forward;
function find_empty_location(grid: IntArrayArray): IntArray; forward;
function sudoku(grid: IntArrayArray): boolean; forward;
procedure print_solution(grid: IntArrayArray); forward;
function is_safe(grid: IntArrayArray; row: integer; column: integer; n: integer): boolean;
var
  is_safe_i: integer;
  is_safe_j: integer;
begin
  for is_safe_i := 0 to (9 - 1) do begin
  if (grid[row][is_safe_i] = n) or (grid[is_safe_i][column] = n) then begin
  exit(false);
end;
end;
  for is_safe_i := 0 to (3 - 1) do begin
  for is_safe_j := 0 to (3 - 1) do begin
  if grid[(row - (row mod 3)) + is_safe_i][(column - (column mod 3)) + is_safe_j] = n then begin
  exit(false);
end;
end;
end;
  exit(true);
end;
function find_empty_location(grid: IntArrayArray): IntArray;
var
  find_empty_location_i: integer;
  find_empty_location_j: integer;
begin
  for find_empty_location_i := 0 to (9 - 1) do begin
  for find_empty_location_j := 0 to (9 - 1) do begin
  if grid[find_empty_location_i][find_empty_location_j] = 0 then begin
  exit([find_empty_location_i, find_empty_location_j]);
end;
end;
end;
  exit([]);
end;
function sudoku(grid: IntArrayArray): boolean;
var
  sudoku_loc: IntArray;
  sudoku_row: integer;
  sudoku_column: integer;
  sudoku_digit: integer;
begin
  sudoku_loc := find_empty_location(grid);
  if Length(sudoku_loc) = 0 then begin
  exit(true);
end;
  sudoku_row := sudoku_loc[0];
  sudoku_column := sudoku_loc[1];
  for sudoku_digit := 1 to (10 - 1) do begin
  if is_safe(grid, sudoku_row, sudoku_column, sudoku_digit) then begin
  grid[sudoku_row][sudoku_column] := sudoku_digit;
  if sudoku(grid) then begin
  exit(true);
end;
  grid[sudoku_row][sudoku_column] := 0;
end;
end;
  exit(false);
end;
procedure print_solution(grid: IntArrayArray);
var
  print_solution_r: integer;
  print_solution_line: string;
  print_solution_c: integer;
begin
  for print_solution_r := 0 to (Length(grid) - 1) do begin
  print_solution_line := '';
  for print_solution_c := 0 to (Length(grid[print_solution_r]) - 1) do begin
  print_solution_line := print_solution_line + IntToStr(grid[print_solution_r][print_solution_c]);
  if print_solution_c < (Length(grid[print_solution_r]) - 1) then begin
  print_solution_line := print_solution_line + ' ';
end;
end;
  writeln(print_solution_line);
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  initial_grid := [[3, 0, 6, 5, 0, 8, 4, 0, 0], [5, 2, 0, 0, 0, 0, 0, 0, 0], [0, 8, 7, 0, 0, 0, 0, 3, 1], [0, 0, 3, 0, 1, 0, 0, 8, 0], [9, 0, 0, 8, 6, 3, 0, 0, 5], [0, 5, 0, 0, 9, 0, 6, 0, 0], [1, 3, 0, 0, 0, 0, 2, 5, 0], [0, 0, 0, 0, 0, 0, 0, 7, 4], [0, 0, 5, 2, 0, 6, 3, 0, 0]];
  no_solution := [[5, 0, 6, 5, 0, 8, 4, 0, 3], [5, 2, 0, 0, 0, 0, 0, 0, 2], [1, 8, 7, 0, 0, 0, 0, 3, 1], [0, 0, 3, 0, 1, 0, 0, 8, 0], [9, 0, 0, 8, 6, 3, 0, 0, 5], [0, 5, 0, 0, 9, 0, 6, 0, 0], [1, 3, 0, 0, 0, 0, 2, 5, 0], [0, 0, 0, 0, 0, 0, 0, 7, 4], [0, 0, 5, 2, 0, 6, 3, 0, 0]];
  examples := [initial_grid, no_solution];
  idx := 0;
  while idx < Length(examples) do begin
  writeln('' + #10 + 'Example grid:' + #10 + '====================');
  print_solution(examples[idx]);
  writeln('' + #10 + 'Example grid solution:');
  if sudoku(examples[idx]) then begin
  print_solution(examples[idx]);
end else begin
  writeln('Cannot find a solution.');
end;
  idx := idx + 1;
end;
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
