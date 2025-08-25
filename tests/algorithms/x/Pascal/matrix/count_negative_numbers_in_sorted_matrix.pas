{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
function list_int_to_str(xs: array of int64): string;
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
  grid: IntArrayArray;
  test_grids: array of IntArrayArray;
  results_bin: array of int64;
  i: int64;
  results_brute: array of int64;
  results_break: array of int64;
function generate_large_matrix(): IntArrayArray; forward;
function find_negative_index(find_negative_index_arr: IntArray): int64; forward;
function count_negatives_binary_search(count_negatives_binary_search_grid: IntArrayArray): int64; forward;
function count_negatives_brute_force(count_negatives_brute_force_grid: IntArrayArray): int64; forward;
function count_negatives_brute_force_with_break(count_negatives_brute_force_with_break_grid: IntArrayArray): int64; forward;
function generate_large_matrix(): IntArrayArray;
var
  generate_large_matrix_result_: array of IntArray;
  generate_large_matrix_i: int64;
  generate_large_matrix_row: array of int64;
  generate_large_matrix_j: int64;
begin
  generate_large_matrix_result_ := [];
  generate_large_matrix_i := 0;
  while generate_large_matrix_i < 1000 do begin
  generate_large_matrix_row := [];
  generate_large_matrix_j := 1000 - generate_large_matrix_i;
  while generate_large_matrix_j > (-1000 - generate_large_matrix_i) do begin
  generate_large_matrix_row := concat(generate_large_matrix_row, IntArray([generate_large_matrix_j]));
  generate_large_matrix_j := generate_large_matrix_j - 1;
end;
  generate_large_matrix_result_ := concat(generate_large_matrix_result_, [generate_large_matrix_row]);
  generate_large_matrix_i := generate_large_matrix_i + 1;
end;
  exit(generate_large_matrix_result_);
end;
function find_negative_index(find_negative_index_arr: IntArray): int64;
var
  find_negative_index_left: int64;
  find_negative_index_right: integer;
  find_negative_index_mid: int64;
  find_negative_index_num: int64;
begin
  find_negative_index_left := 0;
  find_negative_index_right := Length(find_negative_index_arr) - 1;
  if Length(find_negative_index_arr) = 0 then begin
  exit(0);
end;
  if find_negative_index_arr[0] < 0 then begin
  exit(0);
end;
  while find_negative_index_left <= find_negative_index_right do begin
  find_negative_index_mid := _floordiv(find_negative_index_left + find_negative_index_right, 2);
  find_negative_index_num := find_negative_index_arr[find_negative_index_mid];
  if find_negative_index_num < 0 then begin
  if find_negative_index_mid = 0 then begin
  exit(0);
end;
  if find_negative_index_arr[find_negative_index_mid - 1] >= 0 then begin
  exit(find_negative_index_mid);
end;
  find_negative_index_right := find_negative_index_mid - 1;
end else begin
  find_negative_index_left := find_negative_index_mid + 1;
end;
end;
  exit(Length(find_negative_index_arr));
end;
function count_negatives_binary_search(count_negatives_binary_search_grid: IntArrayArray): int64;
var
  count_negatives_binary_search_total: int64;
  count_negatives_binary_search_bound: integer;
  count_negatives_binary_search_i: int64;
  count_negatives_binary_search_row: array of int64;
  count_negatives_binary_search_idx: int64;
begin
  count_negatives_binary_search_total := 0;
  count_negatives_binary_search_bound := Length(count_negatives_binary_search_grid[0]);
  count_negatives_binary_search_i := 0;
  while count_negatives_binary_search_i < Length(count_negatives_binary_search_grid) do begin
  count_negatives_binary_search_row := count_negatives_binary_search_grid[count_negatives_binary_search_i];
  count_negatives_binary_search_idx := find_negative_index(copy(count_negatives_binary_search_row, 0, (count_negatives_binary_search_bound - (0))));
  count_negatives_binary_search_bound := count_negatives_binary_search_idx;
  count_negatives_binary_search_total := count_negatives_binary_search_total + count_negatives_binary_search_idx;
  count_negatives_binary_search_i := count_negatives_binary_search_i + 1;
end;
  exit((Length(count_negatives_binary_search_grid) * Length(count_negatives_binary_search_grid[0])) - count_negatives_binary_search_total);
end;
function count_negatives_brute_force(count_negatives_brute_force_grid: IntArrayArray): int64;
var
  count_negatives_brute_force_count: int64;
  count_negatives_brute_force_i: int64;
  count_negatives_brute_force_row: array of int64;
  count_negatives_brute_force_j: int64;
begin
  count_negatives_brute_force_count := 0;
  count_negatives_brute_force_i := 0;
  while count_negatives_brute_force_i < Length(count_negatives_brute_force_grid) do begin
  count_negatives_brute_force_row := count_negatives_brute_force_grid[count_negatives_brute_force_i];
  count_negatives_brute_force_j := 0;
  while count_negatives_brute_force_j < Length(count_negatives_brute_force_row) do begin
  if count_negatives_brute_force_row[count_negatives_brute_force_j] < 0 then begin
  count_negatives_brute_force_count := count_negatives_brute_force_count + 1;
end;
  count_negatives_brute_force_j := count_negatives_brute_force_j + 1;
end;
  count_negatives_brute_force_i := count_negatives_brute_force_i + 1;
end;
  exit(count_negatives_brute_force_count);
end;
function count_negatives_brute_force_with_break(count_negatives_brute_force_with_break_grid: IntArrayArray): int64;
var
  count_negatives_brute_force_with_break_total: int64;
  count_negatives_brute_force_with_break_i: int64;
  count_negatives_brute_force_with_break_row: array of int64;
  count_negatives_brute_force_with_break_j: int64;
  count_negatives_brute_force_with_break_number: int64;
begin
  count_negatives_brute_force_with_break_total := 0;
  count_negatives_brute_force_with_break_i := 0;
  while count_negatives_brute_force_with_break_i < Length(count_negatives_brute_force_with_break_grid) do begin
  count_negatives_brute_force_with_break_row := count_negatives_brute_force_with_break_grid[count_negatives_brute_force_with_break_i];
  count_negatives_brute_force_with_break_j := 0;
  while count_negatives_brute_force_with_break_j < Length(count_negatives_brute_force_with_break_row) do begin
  count_negatives_brute_force_with_break_number := count_negatives_brute_force_with_break_row[count_negatives_brute_force_with_break_j];
  if count_negatives_brute_force_with_break_number < 0 then begin
  count_negatives_brute_force_with_break_total := count_negatives_brute_force_with_break_total + (Length(count_negatives_brute_force_with_break_row) - count_negatives_brute_force_with_break_j);
  break;
end;
  count_negatives_brute_force_with_break_j := count_negatives_brute_force_with_break_j + 1;
end;
  count_negatives_brute_force_with_break_i := count_negatives_brute_force_with_break_i + 1;
end;
  exit(count_negatives_brute_force_with_break_total);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  grid := generate_large_matrix();
  test_grids := [[[4, 3, 2, -1], [3, 2, 1, -1], [1, 1, -1, -2], [-1, -1, -2, -3]], [[3, 2], [1, 0]], [[7, 7, 6]], [[7, 7, 6], [-1, -2, -3]], grid];
  results_bin := [];
  i := 0;
  while i < Length(test_grids) do begin
  results_bin := concat(results_bin, IntArray([count_negatives_binary_search(test_grids[i])]));
  i := i + 1;
end;
  writeln(list_int_to_str(results_bin));
  results_brute := [];
  i := 0;
  while i < Length(test_grids) do begin
  results_brute := concat(results_brute, IntArray([count_negatives_brute_force(test_grids[i])]));
  i := i + 1;
end;
  writeln(list_int_to_str(results_brute));
  results_break := [];
  i := 0;
  while i < Length(test_grids) do begin
  results_break := concat(results_break, IntArray([count_negatives_brute_force_with_break(test_grids[i])]));
  i := i + 1;
end;
  writeln(list_int_to_str(results_break));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
