{$mode objfpc}{$modeswitch nestedprocvars}
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
function min(xs: array of integer): integer;
var i, m: integer;
begin
  if Length(xs) = 0 then begin min := 0; exit; end;
  m := xs[0];
  for i := 1 to High(xs) do if xs[i] < m then m := xs[i];
  min := m;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  sample: array of IntArray;
  col: integer;
  row: integer;
  largest_square_area: IntArray;
  cols: integer;
  dp_array: IntArrayArray;
  rows: integer;
  mat: IntArrayArray;
function update_area_of_max_square(row: integer; col: integer; rows: integer; cols: integer; mat: IntArrayArray; largest_square_area: IntArray): integer; forward;
function largest_square_area_in_matrix_top_down(rows: integer; cols: integer; mat: IntArrayArray): integer; forward;
function update_area_of_max_square_with_dp(row: integer; col: integer; rows: integer; cols: integer; mat: IntArrayArray; dp_array: IntArrayArray; largest_square_area: IntArray): integer; forward;
function largest_square_area_in_matrix_top_down_with_dp(rows: integer; cols: integer; mat: IntArrayArray): integer; forward;
function largest_square_area_in_matrix_bottom_up(rows: integer; cols: integer; mat: IntArrayArray): integer; forward;
function largest_square_area_in_matrix_bottom_up_space_optimization(rows: integer; cols: integer; mat: IntArrayArray): integer; forward;
function update_area_of_max_square(row: integer; col: integer; rows: integer; cols: integer; mat: IntArrayArray; largest_square_area: IntArray): integer;
var
  update_area_of_max_square_right: integer;
  update_area_of_max_square_diagonal: integer;
  update_area_of_max_square_down: integer;
  update_area_of_max_square_sub: integer;
begin
  if (row >= rows) or (col >= cols) then begin
  exit(0);
end;
  update_area_of_max_square_right := update_area_of_max_square(row, col + 1, rows, cols, mat, largest_square_area);
  update_area_of_max_square_diagonal := update_area_of_max_square(row + 1, col + 1, rows, cols, mat, largest_square_area);
  update_area_of_max_square_down := update_area_of_max_square(row + 1, col, rows, cols, mat, largest_square_area);
  if mat[row][col] = 1 then begin
  update_area_of_max_square_sub := 1 + min([update_area_of_max_square_right, update_area_of_max_square_diagonal, update_area_of_max_square_down]);
  if update_area_of_max_square_sub > largest_square_area[0] then begin
  largest_square_area[0] := update_area_of_max_square_sub;
end;
  exit(update_area_of_max_square_sub);
end else begin
  exit(0);
end;
end;
function largest_square_area_in_matrix_top_down(rows: integer; cols: integer; mat: IntArrayArray): integer;
var
  largest_square_area_in_matrix_top_down_largest: array of integer;
begin
  largest_square_area_in_matrix_top_down_largest := [0];
  update_area_of_max_square(0, 0, rows, cols, mat, largest_square_area_in_matrix_top_down_largest);
  exit(largest_square_area_in_matrix_top_down_largest[0]);
end;
function update_area_of_max_square_with_dp(row: integer; col: integer; rows: integer; cols: integer; mat: IntArrayArray; dp_array: IntArrayArray; largest_square_area: IntArray): integer;
var
  update_area_of_max_square_with_dp_right: integer;
  update_area_of_max_square_with_dp_diagonal: integer;
  update_area_of_max_square_with_dp_down: integer;
  update_area_of_max_square_with_dp_sub: integer;
begin
  if (row >= rows) or (col >= cols) then begin
  exit(0);
end;
  if dp_array[row][col] <> -1 then begin
  exit(dp_array[row][col]);
end;
  update_area_of_max_square_with_dp_right := update_area_of_max_square_with_dp(row, col + 1, rows, cols, mat, dp_array, largest_square_area);
  update_area_of_max_square_with_dp_diagonal := update_area_of_max_square_with_dp(row + 1, col + 1, rows, cols, mat, dp_array, largest_square_area);
  update_area_of_max_square_with_dp_down := update_area_of_max_square_with_dp(row + 1, col, rows, cols, mat, dp_array, largest_square_area);
  if mat[row][col] = 1 then begin
  update_area_of_max_square_with_dp_sub := 1 + min([update_area_of_max_square_with_dp_right, update_area_of_max_square_with_dp_diagonal, update_area_of_max_square_with_dp_down]);
  if update_area_of_max_square_with_dp_sub > largest_square_area[0] then begin
  largest_square_area[0] := update_area_of_max_square_with_dp_sub;
end;
  dp_array[row][col] := update_area_of_max_square_with_dp_sub;
  exit(update_area_of_max_square_with_dp_sub);
end else begin
  dp_array[row][col] := 0;
  exit(0);
end;
end;
function largest_square_area_in_matrix_top_down_with_dp(rows: integer; cols: integer; mat: IntArrayArray): integer;
var
  largest_square_area_in_matrix_top_down_with_dp_largest: array of integer;
  largest_square_area_in_matrix_top_down_with_dp_dp_array: array of IntArray;
  largest_square_area_in_matrix_top_down_with_dp_r: integer;
  largest_square_area_in_matrix_top_down_with_dp_row_list: array of integer;
  largest_square_area_in_matrix_top_down_with_dp_c: integer;
begin
  largest_square_area_in_matrix_top_down_with_dp_largest := [0];
  largest_square_area_in_matrix_top_down_with_dp_dp_array := [];
  largest_square_area_in_matrix_top_down_with_dp_r := 0;
  while largest_square_area_in_matrix_top_down_with_dp_r < rows do begin
  largest_square_area_in_matrix_top_down_with_dp_row_list := [];
  largest_square_area_in_matrix_top_down_with_dp_c := 0;
  while largest_square_area_in_matrix_top_down_with_dp_c < cols do begin
  largest_square_area_in_matrix_top_down_with_dp_row_list := concat(largest_square_area_in_matrix_top_down_with_dp_row_list, IntArray([-1]));
  largest_square_area_in_matrix_top_down_with_dp_c := largest_square_area_in_matrix_top_down_with_dp_c + 1;
end;
  largest_square_area_in_matrix_top_down_with_dp_dp_array := concat(largest_square_area_in_matrix_top_down_with_dp_dp_array, [largest_square_area_in_matrix_top_down_with_dp_row_list]);
  largest_square_area_in_matrix_top_down_with_dp_r := largest_square_area_in_matrix_top_down_with_dp_r + 1;
end;
  update_area_of_max_square_with_dp(0, 0, rows, cols, mat, largest_square_area_in_matrix_top_down_with_dp_dp_array, largest_square_area_in_matrix_top_down_with_dp_largest);
  exit(largest_square_area_in_matrix_top_down_with_dp_largest[0]);
end;
function largest_square_area_in_matrix_bottom_up(rows: integer; cols: integer; mat: IntArrayArray): integer;
var
  largest_square_area_in_matrix_bottom_up_dp_array: array of IntArray;
  largest_square_area_in_matrix_bottom_up_r: integer;
  largest_square_area_in_matrix_bottom_up_row_list: array of integer;
  largest_square_area_in_matrix_bottom_up_c: integer;
  largest_square_area_in_matrix_bottom_up_largest: integer;
  largest_square_area_in_matrix_bottom_up_row: integer;
  largest_square_area_in_matrix_bottom_up_col: integer;
  largest_square_area_in_matrix_bottom_up_right: integer;
  largest_square_area_in_matrix_bottom_up_diagonal: integer;
  largest_square_area_in_matrix_bottom_up_bottom: integer;
  largest_square_area_in_matrix_bottom_up_value: integer;
begin
  largest_square_area_in_matrix_bottom_up_dp_array := [];
  largest_square_area_in_matrix_bottom_up_r := 0;
  while largest_square_area_in_matrix_bottom_up_r <= rows do begin
  largest_square_area_in_matrix_bottom_up_row_list := [];
  largest_square_area_in_matrix_bottom_up_c := 0;
  while largest_square_area_in_matrix_bottom_up_c <= cols do begin
  largest_square_area_in_matrix_bottom_up_row_list := concat(largest_square_area_in_matrix_bottom_up_row_list, IntArray([0]));
  largest_square_area_in_matrix_bottom_up_c := largest_square_area_in_matrix_bottom_up_c + 1;
end;
  largest_square_area_in_matrix_bottom_up_dp_array := concat(largest_square_area_in_matrix_bottom_up_dp_array, [largest_square_area_in_matrix_bottom_up_row_list]);
  largest_square_area_in_matrix_bottom_up_r := largest_square_area_in_matrix_bottom_up_r + 1;
end;
  largest_square_area_in_matrix_bottom_up_largest := 0;
  largest_square_area_in_matrix_bottom_up_row := rows - 1;
  while largest_square_area_in_matrix_bottom_up_row >= 0 do begin
  largest_square_area_in_matrix_bottom_up_col := cols - 1;
  while largest_square_area_in_matrix_bottom_up_col >= 0 do begin
  largest_square_area_in_matrix_bottom_up_right := largest_square_area_in_matrix_bottom_up_dp_array[largest_square_area_in_matrix_bottom_up_row][largest_square_area_in_matrix_bottom_up_col + 1];
  largest_square_area_in_matrix_bottom_up_diagonal := largest_square_area_in_matrix_bottom_up_dp_array[largest_square_area_in_matrix_bottom_up_row + 1][largest_square_area_in_matrix_bottom_up_col + 1];
  largest_square_area_in_matrix_bottom_up_bottom := largest_square_area_in_matrix_bottom_up_dp_array[largest_square_area_in_matrix_bottom_up_row + 1][largest_square_area_in_matrix_bottom_up_col];
  if mat[largest_square_area_in_matrix_bottom_up_row][largest_square_area_in_matrix_bottom_up_col] = 1 then begin
  largest_square_area_in_matrix_bottom_up_value := 1 + min([largest_square_area_in_matrix_bottom_up_right, largest_square_area_in_matrix_bottom_up_diagonal, largest_square_area_in_matrix_bottom_up_bottom]);
  largest_square_area_in_matrix_bottom_up_dp_array[largest_square_area_in_matrix_bottom_up_row][largest_square_area_in_matrix_bottom_up_col] := largest_square_area_in_matrix_bottom_up_value;
  if largest_square_area_in_matrix_bottom_up_value > largest_square_area_in_matrix_bottom_up_largest then begin
  largest_square_area_in_matrix_bottom_up_largest := largest_square_area_in_matrix_bottom_up_value;
end;
end else begin
  largest_square_area_in_matrix_bottom_up_dp_array[largest_square_area_in_matrix_bottom_up_row][largest_square_area_in_matrix_bottom_up_col] := 0;
end;
  largest_square_area_in_matrix_bottom_up_col := largest_square_area_in_matrix_bottom_up_col - 1;
end;
  largest_square_area_in_matrix_bottom_up_row := largest_square_area_in_matrix_bottom_up_row - 1;
end;
  exit(largest_square_area_in_matrix_bottom_up_largest);
end;
function largest_square_area_in_matrix_bottom_up_space_optimization(rows: integer; cols: integer; mat: IntArrayArray): integer;
var
  largest_square_area_in_matrix_bottom_up_space_optimization_current_row: array of integer;
  largest_square_area_in_matrix_bottom_up_space_optimization_i: integer;
  largest_square_area_in_matrix_bottom_up_space_optimization_next_row: array of integer;
  largest_square_area_in_matrix_bottom_up_space_optimization_j: integer;
  largest_square_area_in_matrix_bottom_up_space_optimization_largest: integer;
  largest_square_area_in_matrix_bottom_up_space_optimization_row: integer;
  largest_square_area_in_matrix_bottom_up_space_optimization_col: integer;
  largest_square_area_in_matrix_bottom_up_space_optimization_right: integer;
  largest_square_area_in_matrix_bottom_up_space_optimization_diagonal: integer;
  largest_square_area_in_matrix_bottom_up_space_optimization_bottom: integer;
  largest_square_area_in_matrix_bottom_up_space_optimization_value: integer;
  largest_square_area_in_matrix_bottom_up_space_optimization_t: integer;
begin
  largest_square_area_in_matrix_bottom_up_space_optimization_current_row := [];
  largest_square_area_in_matrix_bottom_up_space_optimization_i := 0;
  while largest_square_area_in_matrix_bottom_up_space_optimization_i <= cols do begin
  largest_square_area_in_matrix_bottom_up_space_optimization_current_row := concat(largest_square_area_in_matrix_bottom_up_space_optimization_current_row, IntArray([0]));
  largest_square_area_in_matrix_bottom_up_space_optimization_i := largest_square_area_in_matrix_bottom_up_space_optimization_i + 1;
end;
  largest_square_area_in_matrix_bottom_up_space_optimization_next_row := [];
  largest_square_area_in_matrix_bottom_up_space_optimization_j := 0;
  while largest_square_area_in_matrix_bottom_up_space_optimization_j <= cols do begin
  largest_square_area_in_matrix_bottom_up_space_optimization_next_row := concat(largest_square_area_in_matrix_bottom_up_space_optimization_next_row, IntArray([0]));
  largest_square_area_in_matrix_bottom_up_space_optimization_j := largest_square_area_in_matrix_bottom_up_space_optimization_j + 1;
end;
  largest_square_area_in_matrix_bottom_up_space_optimization_largest := 0;
  largest_square_area_in_matrix_bottom_up_space_optimization_row := rows - 1;
  while largest_square_area_in_matrix_bottom_up_space_optimization_row >= 0 do begin
  largest_square_area_in_matrix_bottom_up_space_optimization_col := cols - 1;
  while largest_square_area_in_matrix_bottom_up_space_optimization_col >= 0 do begin
  largest_square_area_in_matrix_bottom_up_space_optimization_right := largest_square_area_in_matrix_bottom_up_space_optimization_current_row[largest_square_area_in_matrix_bottom_up_space_optimization_col + 1];
  largest_square_area_in_matrix_bottom_up_space_optimization_diagonal := largest_square_area_in_matrix_bottom_up_space_optimization_next_row[largest_square_area_in_matrix_bottom_up_space_optimization_col + 1];
  largest_square_area_in_matrix_bottom_up_space_optimization_bottom := largest_square_area_in_matrix_bottom_up_space_optimization_next_row[largest_square_area_in_matrix_bottom_up_space_optimization_col];
  if mat[largest_square_area_in_matrix_bottom_up_space_optimization_row][largest_square_area_in_matrix_bottom_up_space_optimization_col] = 1 then begin
  largest_square_area_in_matrix_bottom_up_space_optimization_value := 1 + min([largest_square_area_in_matrix_bottom_up_space_optimization_right, largest_square_area_in_matrix_bottom_up_space_optimization_diagonal, largest_square_area_in_matrix_bottom_up_space_optimization_bottom]);
  largest_square_area_in_matrix_bottom_up_space_optimization_current_row[largest_square_area_in_matrix_bottom_up_space_optimization_col] := largest_square_area_in_matrix_bottom_up_space_optimization_value;
  if largest_square_area_in_matrix_bottom_up_space_optimization_value > largest_square_area_in_matrix_bottom_up_space_optimization_largest then begin
  largest_square_area_in_matrix_bottom_up_space_optimization_largest := largest_square_area_in_matrix_bottom_up_space_optimization_value;
end;
end else begin
  largest_square_area_in_matrix_bottom_up_space_optimization_current_row[largest_square_area_in_matrix_bottom_up_space_optimization_col] := 0;
end;
  largest_square_area_in_matrix_bottom_up_space_optimization_col := largest_square_area_in_matrix_bottom_up_space_optimization_col - 1;
end;
  largest_square_area_in_matrix_bottom_up_space_optimization_next_row := largest_square_area_in_matrix_bottom_up_space_optimization_current_row;
  largest_square_area_in_matrix_bottom_up_space_optimization_current_row := [];
  largest_square_area_in_matrix_bottom_up_space_optimization_t := 0;
  while largest_square_area_in_matrix_bottom_up_space_optimization_t <= cols do begin
  largest_square_area_in_matrix_bottom_up_space_optimization_current_row := concat(largest_square_area_in_matrix_bottom_up_space_optimization_current_row, IntArray([0]));
  largest_square_area_in_matrix_bottom_up_space_optimization_t := largest_square_area_in_matrix_bottom_up_space_optimization_t + 1;
end;
  largest_square_area_in_matrix_bottom_up_space_optimization_row := largest_square_area_in_matrix_bottom_up_space_optimization_row - 1;
end;
  exit(largest_square_area_in_matrix_bottom_up_space_optimization_largest);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  sample := [[1, 1], [1, 1]];
  writeln(largest_square_area_in_matrix_top_down(2, 2, sample));
  writeln(largest_square_area_in_matrix_top_down_with_dp(2, 2, sample));
  writeln(largest_square_area_in_matrix_bottom_up(2, 2, sample));
  writeln(largest_square_area_in_matrix_bottom_up_space_optimization(2, 2, sample));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
