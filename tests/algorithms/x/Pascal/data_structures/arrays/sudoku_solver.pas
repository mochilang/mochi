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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  puzzle: string;
  grid: IntArrayArray;
  column: integer;
  row: integer;
  n: integer;
  s: string;
function string_to_grid(s: string): IntArrayArray; forward;
procedure print_grid(grid: IntArrayArray); forward;
function is_safe(grid: IntArrayArray; row: integer; column: integer; n: integer): boolean; forward;
function find_empty(grid: IntArrayArray): IntArray; forward;
function solve(grid: IntArrayArray): boolean; forward;
function string_to_grid(s: string): IntArrayArray;
var
  string_to_grid_grid: array of IntArray;
  string_to_grid_i: integer;
  string_to_grid_row: array of integer;
  string_to_grid_j: integer;
  string_to_grid_ch: string;
  string_to_grid_val: integer;
begin
  string_to_grid_grid := [];
  string_to_grid_i := 0;
  while string_to_grid_i < 9 do begin
  string_to_grid_row := [];
  string_to_grid_j := 0;
  while string_to_grid_j < 9 do begin
  string_to_grid_ch := copy(s, (string_to_grid_i * 9) + string_to_grid_j+1, (((string_to_grid_i * 9) + string_to_grid_j) + 1 - ((string_to_grid_i * 9) + string_to_grid_j)));
  string_to_grid_val := 0;
  if (string_to_grid_ch <> '0') and (string_to_grid_ch <> '.') then begin
  string_to_grid_val := StrToInt(string_to_grid_ch);
end;
  string_to_grid_row := concat(string_to_grid_row, IntArray([string_to_grid_val]));
  string_to_grid_j := string_to_grid_j + 1;
end;
  string_to_grid_grid := concat(string_to_grid_grid, [string_to_grid_row]);
  string_to_grid_i := string_to_grid_i + 1;
end;
  exit(string_to_grid_grid);
end;
procedure print_grid(grid: IntArrayArray);
var
  print_grid_r: int64;
  print_grid_line: string;
  print_grid_c: int64;
begin
  for print_grid_r := 0 to (9 - 1) do begin
  print_grid_line := '';
  for print_grid_c := 0 to (9 - 1) do begin
  print_grid_line := print_grid_line + IntToStr(grid[print_grid_r][print_grid_c]);
  if print_grid_c < 8 then begin
  print_grid_line := print_grid_line + ' ';
end;
end;
  writeln(print_grid_line);
end;
end;
function is_safe(grid: IntArrayArray; row: integer; column: integer; n: integer): boolean;
var
  is_safe_i: int64;
  is_safe_j: int64;
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
function find_empty(grid: IntArrayArray): IntArray;
var
  find_empty_i: int64;
  find_empty_j: int64;
begin
  for find_empty_i := 0 to (9 - 1) do begin
  for find_empty_j := 0 to (9 - 1) do begin
  if grid[find_empty_i][find_empty_j] = 0 then begin
  exit([find_empty_i, find_empty_j]);
end;
end;
end;
  exit([]);
end;
function solve(grid: IntArrayArray): boolean;
var
  solve_loc: IntArray;
  solve_row: integer;
  solve_column: integer;
  solve_digit: int64;
begin
  solve_loc := find_empty(grid);
  if Length(solve_loc) = 0 then begin
  exit(true);
end;
  solve_row := solve_loc[0];
  solve_column := solve_loc[1];
  for solve_digit := 1 to (10 - 1) do begin
  if is_safe(grid, solve_row, solve_column, solve_digit) then begin
  grid[solve_row][solve_column] := solve_digit;
  if solve(grid) then begin
  exit(true);
end;
  grid[solve_row][solve_column] := 0;
end;
end;
  exit(false);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  puzzle := '003020600900305001001806400008102900700000008006708200002609500800203009005010300';
  grid := string_to_grid(puzzle);
  writeln('Original grid:');
  print_grid(grid);
  if solve(grid) then begin
  writeln('' + #10 + 'Solved grid:');
  print_grid(grid);
end else begin
  writeln('' + #10 + 'No solution found');
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
