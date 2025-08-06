{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
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
function list_to_str(xs: array of string): string;
var i: integer;
begin
  Result := '#(' + sLineBreak;
  for i := 0 to High(xs) do begin
    Result := Result + '  ''' + xs[i] + '''.' + sLineBreak;
  end;
  Result := Result + ')';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  no_of_variable: integer;
  row: IntArray;
  ch: string;
  s: string;
  string2: string;
  minterms: IntArray;
  arr: StrArray;
  count: integer;
  chart: IntArrayArray;
  binary: StrArray;
  string1: string;
  value: string;
  prime_implicants: StrArray;
function compare_string(string1: string; string2: string): string; forward;
function contains_string(arr: StrArray; value: string): boolean; forward;
function unique_strings(arr: StrArray): StrArray; forward;
function check(binary: StrArray): StrArray; forward;
function decimal_to_binary(no_of_variable: integer; minterms: IntArray): StrArray; forward;
function is_for_table(string1: string; string2: string; count: integer): boolean; forward;
function count_ones(row: IntArray): integer; forward;
function selection(chart: IntArrayArray; prime_implicants: StrArray): StrArray; forward;
function count_char(s: string; ch: string): integer; forward;
function prime_implicant_chart(prime_implicants: StrArray; binary: StrArray): IntArrayArray; forward;
procedure main(); forward;
function compare_string(string1: string; string2: string): string;
var
  compare_string_result_: string;
  compare_string_count: integer;
  compare_string_i: integer;
  compare_string_c1: string;
  compare_string_c2: string;
begin
  compare_string_result_ := '';
  compare_string_count := 0;
  compare_string_i := 0;
  while compare_string_i < Length(string1) do begin
  compare_string_c1 := copy(string1, compare_string_i+1, (compare_string_i + 1 - (compare_string_i)));
  compare_string_c2 := copy(string2, compare_string_i+1, (compare_string_i + 1 - (compare_string_i)));
  if compare_string_c1 <> compare_string_c2 then begin
  compare_string_count := compare_string_count + 1;
  compare_string_result_ := compare_string_result_ + '_';
end else begin
  compare_string_result_ := compare_string_result_ + compare_string_c1;
end;
  compare_string_i := compare_string_i + 1;
end;
  if compare_string_count > 1 then begin
  exit('');
end;
  exit(compare_string_result_);
end;
function contains_string(arr: StrArray; value: string): boolean;
var
  contains_string_i: integer;
begin
  contains_string_i := 0;
  while contains_string_i < Length(arr) do begin
  if arr[contains_string_i] = value then begin
  exit(true);
end;
  contains_string_i := contains_string_i + 1;
end;
  exit(false);
end;
function unique_strings(arr: StrArray): StrArray;
var
  unique_strings_res: array of string;
  unique_strings_i: integer;
begin
  unique_strings_res := [];
  unique_strings_i := 0;
  while unique_strings_i < Length(arr) do begin
  if not contains_string(unique_strings_res, arr[unique_strings_i]) then begin
  unique_strings_res := concat(unique_strings_res, StrArray([arr[unique_strings_i]]));
end;
  unique_strings_i := unique_strings_i + 1;
end;
  exit(unique_strings_res);
end;
function check(binary: StrArray): StrArray;
var
  check_pi: array of string;
  check_current: array of string;
  check_check1: array of string;
  check_i: integer;
  check_temp: array of string;
  check_j: integer;
  check_k: string;
begin
  check_pi := [];
  check_current := binary;
  while true do begin
  check_check1 := [];
  check_i := 0;
  while check_i < Length(check_current) do begin
  check_check1 := concat(check_check1, StrArray(['$']));
  check_i := check_i + 1;
end;
  check_temp := [];
  check_i := 0;
  while check_i < Length(check_current) do begin
  check_j := check_i + 1;
  while check_j < Length(check_current) do begin
  check_k := compare_string(check_current[check_i], check_current[check_j]);
  if check_k = '' then begin
  check_check1[check_i] := '*';
  check_check1[check_j] := '*';
  check_temp := concat(check_temp, StrArray(['X']));
end;
  check_j := check_j + 1;
end;
  check_i := check_i + 1;
end;
  check_i := 0;
  while check_i < Length(check_current) do begin
  if check_check1[check_i] = '$' then begin
  check_pi := concat(check_pi, StrArray([check_current[check_i]]));
end;
  check_i := check_i + 1;
end;
  if Length(check_temp) = 0 then begin
  exit(check_pi);
end;
  check_current := unique_strings(check_temp);
end;
end;
function decimal_to_binary(no_of_variable: integer; minterms: IntArray): StrArray;
var
  decimal_to_binary_temp: array of string;
  decimal_to_binary_idx: integer;
  decimal_to_binary_minterm: integer;
  decimal_to_binary_string: string;
  decimal_to_binary_i: integer;
begin
  decimal_to_binary_temp := [];
  decimal_to_binary_idx := 0;
  while decimal_to_binary_idx < Length(minterms) do begin
  decimal_to_binary_minterm := minterms[decimal_to_binary_idx];
  decimal_to_binary_string := '';
  decimal_to_binary_i := 0;
  while decimal_to_binary_i < no_of_variable do begin
  decimal_to_binary_string := IntToStr(decimal_to_binary_minterm mod 2) + decimal_to_binary_string;
  decimal_to_binary_minterm := decimal_to_binary_minterm div 2;
  decimal_to_binary_i := decimal_to_binary_i + 1;
end;
  decimal_to_binary_temp := concat(decimal_to_binary_temp, StrArray([decimal_to_binary_string]));
  decimal_to_binary_idx := decimal_to_binary_idx + 1;
end;
  exit(decimal_to_binary_temp);
end;
function is_for_table(string1: string; string2: string; count: integer): boolean;
var
  is_for_table_count_n: integer;
  is_for_table_i: integer;
  is_for_table_c1: string;
  is_for_table_c2: string;
begin
  is_for_table_count_n := 0;
  is_for_table_i := 0;
  while is_for_table_i < Length(string1) do begin
  is_for_table_c1 := copy(string1, is_for_table_i+1, (is_for_table_i + 1 - (is_for_table_i)));
  is_for_table_c2 := copy(string2, is_for_table_i+1, (is_for_table_i + 1 - (is_for_table_i)));
  if is_for_table_c1 <> is_for_table_c2 then begin
  is_for_table_count_n := is_for_table_count_n + 1;
end;
  is_for_table_i := is_for_table_i + 1;
end;
  exit(is_for_table_count_n = count);
end;
function count_ones(row: IntArray): integer;
var
  count_ones_c: integer;
  count_ones_j: integer;
begin
  count_ones_c := 0;
  count_ones_j := 0;
  while count_ones_j < Length(row) do begin
  if row[count_ones_j] = 1 then begin
  count_ones_c := count_ones_c + 1;
end;
  count_ones_j := count_ones_j + 1;
end;
  exit(count_ones_c);
end;
function selection(chart: IntArrayArray; prime_implicants: StrArray): StrArray;
var
  selection_temp: array of string;
  selection_select: array of integer;
  selection_i: integer;
  selection_col: integer;
  selection_count: integer;
  selection_row: integer;
  selection_rem: integer;
  selection_j: integer;
  selection_r: integer;
  selection_counts: array of integer;
  selection_max_n: integer;
  selection_k: integer;
  selection_r2: integer;
begin
  selection_temp := [];
  selection_select := [];
  selection_i := 0;
  while selection_i < Length(chart) do begin
  selection_select := concat(selection_select, IntArray([0]));
  selection_i := selection_i + 1;
end;
  selection_col := 0;
  while selection_col < Length(chart[0]) do begin
  selection_count := 0;
  selection_row := 0;
  while selection_row < Length(chart) do begin
  if chart[selection_row][selection_col] = 1 then begin
  selection_count := selection_count + 1;
end;
  selection_row := selection_row + 1;
end;
  if selection_count = 1 then begin
  selection_rem := 0;
  selection_row := 0;
  while selection_row < Length(chart) do begin
  if chart[selection_row][selection_col] = 1 then begin
  selection_rem := selection_row;
end;
  selection_row := selection_row + 1;
end;
  selection_select[selection_rem] := 1;
end;
  selection_col := selection_col + 1;
end;
  selection_i := 0;
  while selection_i < Length(selection_select) do begin
  if selection_select[selection_i] = 1 then begin
  selection_j := 0;
  while selection_j < Length(chart[0]) do begin
  if chart[selection_i][selection_j] = 1 then begin
  selection_r := 0;
  while selection_r < Length(chart) do begin
  chart[selection_r][selection_j] := 0;
  selection_r := selection_r + 1;
end;
end;
  selection_j := selection_j + 1;
end;
  selection_temp := concat(selection_temp, StrArray([prime_implicants[selection_i]]));
end;
  selection_i := selection_i + 1;
end;
  while true do begin
  selection_counts := [];
  selection_r := 0;
  while selection_r < Length(chart) do begin
  selection_counts := concat(selection_counts, IntArray([count_ones(chart[selection_r])]));
  selection_r := selection_r + 1;
end;
  selection_max_n := selection_counts[0];
  selection_rem := 0;
  selection_k := 1;
  while selection_k < Length(selection_counts) do begin
  if selection_counts[selection_k] > selection_max_n then begin
  selection_max_n := selection_counts[selection_k];
  selection_rem := selection_k;
end;
  selection_k := selection_k + 1;
end;
  if selection_max_n = 0 then begin
  exit(selection_temp);
end;
  selection_temp := concat(selection_temp, StrArray([prime_implicants[selection_rem]]));
  selection_j := 0;
  while selection_j < Length(chart[0]) do begin
  if chart[selection_rem][selection_j] = 1 then begin
  selection_r2 := 0;
  while selection_r2 < Length(chart) do begin
  chart[selection_r2][selection_j] := 0;
  selection_r2 := selection_r2 + 1;
end;
end;
  selection_j := selection_j + 1;
end;
end;
end;
function count_char(s: string; ch: string): integer;
var
  count_char_cnt: integer;
  count_char_i: integer;
begin
  count_char_cnt := 0;
  count_char_i := 0;
  while count_char_i < Length(s) do begin
  if copy(s, count_char_i+1, (count_char_i + 1 - (count_char_i))) = ch then begin
  count_char_cnt := count_char_cnt + 1;
end;
  count_char_i := count_char_i + 1;
end;
  exit(count_char_cnt);
end;
function prime_implicant_chart(prime_implicants: StrArray; binary: StrArray): IntArrayArray;
var
  prime_implicant_chart_chart: array of IntArray;
  prime_implicant_chart_i: integer;
  prime_implicant_chart_row: array of integer;
  prime_implicant_chart_j: integer;
  prime_implicant_chart_count: integer;
begin
  prime_implicant_chart_chart := [];
  prime_implicant_chart_i := 0;
  while prime_implicant_chart_i < Length(prime_implicants) do begin
  prime_implicant_chart_row := [];
  prime_implicant_chart_j := 0;
  while prime_implicant_chart_j < Length(binary) do begin
  prime_implicant_chart_row := concat(prime_implicant_chart_row, IntArray([0]));
  prime_implicant_chart_j := prime_implicant_chart_j + 1;
end;
  prime_implicant_chart_chart := concat(prime_implicant_chart_chart, [prime_implicant_chart_row]);
  prime_implicant_chart_i := prime_implicant_chart_i + 1;
end;
  prime_implicant_chart_i := 0;
  while prime_implicant_chart_i < Length(prime_implicants) do begin
  prime_implicant_chart_count := count_char(prime_implicants[prime_implicant_chart_i], '_');
  prime_implicant_chart_j := 0;
  while prime_implicant_chart_j < Length(binary) do begin
  if is_for_table(prime_implicants[prime_implicant_chart_i], binary[prime_implicant_chart_j], prime_implicant_chart_count) then begin
  prime_implicant_chart_chart[prime_implicant_chart_i][prime_implicant_chart_j] := 1;
end;
  prime_implicant_chart_j := prime_implicant_chart_j + 1;
end;
  prime_implicant_chart_i := prime_implicant_chart_i + 1;
end;
  exit(prime_implicant_chart_chart);
end;
procedure main();
var
  main_no_of_variable: integer;
  main_minterms: array of integer;
  main_binary: StrArray;
  main_prime_implicants: StrArray;
  main_chart: IntArrayArray;
  main_essential_prime_implicants: StrArray;
begin
  main_no_of_variable := 3;
  main_minterms := [1, 5, 7];
  main_binary := decimal_to_binary(main_no_of_variable, main_minterms);
  main_prime_implicants := check(main_binary);
  writeln('Prime Implicants are:');
  writeln(list_to_str(main_prime_implicants));
  main_chart := prime_implicant_chart(main_prime_implicants, main_binary);
  main_essential_prime_implicants := selection(main_chart, main_prime_implicants);
  writeln('Essential Prime Implicants are:');
  writeln(list_to_str(main_essential_prime_implicants));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
