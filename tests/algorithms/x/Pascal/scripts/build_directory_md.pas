{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
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
  sample: array of string;
  name: string;
  old_path: string;
  level: integer;
  sep: string;
  new: string;
  n: integer;
  paths: StrArray;
  s: string;
  xs: StrArray;
  old: string;
  ch: string;
  new_path: string;
  sub: string;
function split(s: string; sep: string): StrArray; forward;
function join(xs: StrArray; sep: string): string; forward;
function repeat_(s: string; n: integer): string; forward;
function replace_char(s: string; old: string; new: string): string; forward;
function contains(s: string; sub: string): boolean; forward;
function file_extension(name: string): string; forward;
function remove_extension(name: string): string; forward;
function title_case(s: string): string; forward;
function count_char(s: string; ch: string): integer; forward;
function md_prefix(level: integer): string; forward;
function print_path(old_path: string; new_path: string): string; forward;
function sort_strings(xs: StrArray): StrArray; forward;
function good_file_paths(paths: StrArray): StrArray; forward;
procedure print_directory_md(paths: StrArray); forward;
function split(s: string; sep: string): StrArray;
var
  split_parts: array of string;
  split_cur: string;
  split_i: integer;
begin
  split_parts := [];
  split_cur := '';
  split_i := 0;
  while split_i < Length(s) do begin
  if ((Length(sep) > 0) and ((split_i + Length(sep)) <= Length(s))) and (copy(s, split_i+1, (split_i + Length(sep) - (split_i))) = sep) then begin
  split_parts := concat(split_parts, StrArray([split_cur]));
  split_cur := '';
  split_i := split_i + Length(sep);
end else begin
  split_cur := split_cur + copy(s, split_i+1, (split_i + 1 - (split_i)));
  split_i := split_i + 1;
end;
end;
  split_parts := concat(split_parts, StrArray([split_cur]));
  exit(split_parts);
end;
function join(xs: StrArray; sep: string): string;
var
  join_res: string;
  join_i: integer;
begin
  join_res := '';
  join_i := 0;
  while join_i < Length(xs) do begin
  if join_i > 0 then begin
  join_res := join_res + sep;
end;
  join_res := join_res + xs[join_i];
  join_i := join_i + 1;
end;
  exit(join_res);
end;
function repeat_(s: string; n: integer): string;
var
  repeat__out: string;
  repeat__i: integer;
begin
  repeat__out := '';
  repeat__i := 0;
  while repeat__i < n do begin
  repeat__out := repeat__out + s;
  repeat__i := repeat__i + 1;
end;
  exit(repeat__out);
end;
function replace_char(s: string; old: string; new: string): string;
var
  replace_char_out: string;
  replace_char_i: integer;
  replace_char_c: string;
begin
  replace_char_out := '';
  replace_char_i := 0;
  while replace_char_i < Length(s) do begin
  replace_char_c := copy(s, replace_char_i+1, (replace_char_i + 1 - (replace_char_i)));
  if replace_char_c = old then begin
  replace_char_out := replace_char_out + new;
end else begin
  replace_char_out := replace_char_out + replace_char_c;
end;
  replace_char_i := replace_char_i + 1;
end;
  exit(replace_char_out);
end;
function contains(s: string; sub: string): boolean;
var
  contains_i: integer;
begin
  if Length(sub) = 0 then begin
  exit(true);
end;
  contains_i := 0;
  while (contains_i + Length(sub)) <= Length(s) do begin
  if copy(s, contains_i+1, (contains_i + Length(sub) - (contains_i))) = sub then begin
  exit(true);
end;
  contains_i := contains_i + 1;
end;
  exit(false);
end;
function file_extension(name: string): string;
var
  file_extension_i: integer;
begin
  file_extension_i := Length(name) - 1;
  while file_extension_i >= 0 do begin
  if copy(name, file_extension_i+1, (file_extension_i + 1 - (file_extension_i))) = '.' then begin
  exit(copy(name, file_extension_i+1, Length(name)));
end;
  file_extension_i := file_extension_i - 1;
end;
  exit('');
end;
function remove_extension(name: string): string;
var
  remove_extension_i: integer;
begin
  remove_extension_i := Length(name) - 1;
  while remove_extension_i >= 0 do begin
  if copy(name, remove_extension_i+1, (remove_extension_i + 1 - (remove_extension_i))) = '.' then begin
  exit(copy(name, 1, remove_extension_i));
end;
  remove_extension_i := remove_extension_i - 1;
end;
  exit(name);
end;
function title_case(s: string): string;
var
  title_case_out: string;
  title_case_cap: boolean;
  title_case_i: integer;
  title_case_c: string;
begin
  title_case_out := '';
  title_case_cap := true;
  title_case_i := 0;
  while title_case_i < Length(s) do begin
  title_case_c := copy(s, title_case_i+1, (title_case_i + 1 - (title_case_i)));
  if title_case_c = ' ' then begin
  title_case_out := title_case_out + title_case_c;
  title_case_cap := true;
end else begin
  if title_case_cap then begin
  title_case_out := title_case_out + UpperCase(title_case_c);
  title_case_cap := false;
end else begin
  title_case_out := title_case_out + LowerCase(title_case_c);
end;
end;
  title_case_i := title_case_i + 1;
end;
  exit(title_case_out);
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
function md_prefix(level: integer): string;
begin
  if level = 0 then begin
  exit('' + #10 + '##');
end;
  exit(repeat_('  ', level) + '*');
end;
function print_path(old_path: string; new_path: string): string;
var
  print_path_old_parts: StrArray;
  print_path_new_parts: StrArray;
  print_path_i: integer;
  print_path_title: string;
begin
  print_path_old_parts := split(old_path, '/');
  print_path_new_parts := split(new_path, '/');
  print_path_i := 0;
  while print_path_i < Length(print_path_new_parts) do begin
  if ((print_path_i >= Length(print_path_old_parts)) or (print_path_old_parts[print_path_i] <> print_path_new_parts[print_path_i])) and (print_path_new_parts[print_path_i] <> '') then begin
  print_path_title := title_case(replace_char(print_path_new_parts[print_path_i], '_', ' '));
  writeln((md_prefix(print_path_i) + ' ') + print_path_title);
end;
  print_path_i := print_path_i + 1;
end;
  exit(new_path);
end;
function sort_strings(xs: StrArray): StrArray;
var
  sort_strings_arr: array of string;
  sort_strings_i: integer;
  sort_strings_min_idx: integer;
  sort_strings_j: integer;
  sort_strings_tmp: string;
begin
  sort_strings_arr := xs;
  sort_strings_i := 0;
  while sort_strings_i < Length(sort_strings_arr) do begin
  sort_strings_min_idx := sort_strings_i;
  sort_strings_j := sort_strings_i + 1;
  while sort_strings_j < Length(sort_strings_arr) do begin
  if sort_strings_arr[sort_strings_j] < sort_strings_arr[sort_strings_min_idx] then begin
  sort_strings_min_idx := sort_strings_j;
end;
  sort_strings_j := sort_strings_j + 1;
end;
  sort_strings_tmp := sort_strings_arr[sort_strings_i];
  sort_strings_arr[sort_strings_i] := sort_strings_arr[sort_strings_min_idx];
  sort_strings_arr[sort_strings_min_idx] := sort_strings_tmp;
  sort_strings_i := sort_strings_i + 1;
end;
  exit(sort_strings_arr);
end;
function good_file_paths(paths: StrArray): StrArray;
var
  good_file_paths_res: array of string;
  good_file_paths_p: string;
  good_file_paths_parts: StrArray;
  good_file_paths_skip: boolean;
  good_file_paths_k: integer;
  good_file_paths_part: string;
  good_file_paths_filename: string;
  good_file_paths_ext: string;
begin
  good_file_paths_res := [];
  for good_file_paths_p in paths do begin
  good_file_paths_parts := split(good_file_paths_p, '/');
  good_file_paths_skip := false;
  good_file_paths_k := 0;
  while good_file_paths_k < (Length(good_file_paths_parts) - 1) do begin
  good_file_paths_part := good_file_paths_parts[good_file_paths_k];
  if (((good_file_paths_part = 'scripts') or (copy(good_file_paths_part, 1, 1) = '.')) or (copy(good_file_paths_part, 1, 1) = '_')) or contains(good_file_paths_part, 'venv') then begin
  good_file_paths_skip := true;
end;
  good_file_paths_k := good_file_paths_k + 1;
end;
  if good_file_paths_skip then begin
  continue;
end;
  good_file_paths_filename := good_file_paths_parts[Length(good_file_paths_parts) - 1];
  if good_file_paths_filename = '__init__.py' then begin
  continue;
end;
  good_file_paths_ext := file_extension(good_file_paths_filename);
  if (good_file_paths_ext = '.py') or (good_file_paths_ext = '.ipynb') then begin
  good_file_paths_res := concat(good_file_paths_res, StrArray([good_file_paths_p]));
end;
end;
  exit(good_file_paths_res);
end;
procedure print_directory_md(paths: StrArray);
var
  print_directory_md_files: StrArray;
  print_directory_md_old_path: string;
  print_directory_md_i: integer;
  print_directory_md_fp: string;
  print_directory_md_parts: StrArray;
  print_directory_md_filename: string;
  print_directory_md_filepath: string;
  print_directory_md_indent: integer;
  print_directory_md_url: string;
  print_directory_md_name: string;
begin
  print_directory_md_files := sort_strings(good_file_paths(paths));
  print_directory_md_old_path := '';
  print_directory_md_i := 0;
  while print_directory_md_i < Length(print_directory_md_files) do begin
  print_directory_md_fp := print_directory_md_files[print_directory_md_i];
  print_directory_md_parts := split(print_directory_md_fp, '/');
  print_directory_md_filename := print_directory_md_parts[Length(print_directory_md_parts) - 1];
  print_directory_md_filepath := '';
  if Length(print_directory_md_parts) > 1 then begin
  print_directory_md_filepath := join(copy(print_directory_md_parts, 0, Length(print_directory_md_parts) - 1), '/');
end;
  if print_directory_md_filepath <> print_directory_md_old_path then begin
  print_directory_md_old_path := print_path(print_directory_md_old_path, print_directory_md_filepath);
end;
  print_directory_md_indent := 0;
  if Length(print_directory_md_filepath) > 0 then begin
  print_directory_md_indent := count_char(print_directory_md_filepath, '/') + 1;
end;
  print_directory_md_url := replace_char(print_directory_md_fp, ' ', '%20');
  print_directory_md_name := title_case(replace_char(remove_extension(print_directory_md_filename), '_', ' '));
  writeln(((((md_prefix(print_directory_md_indent) + ' [') + print_directory_md_name) + '](') + print_directory_md_url) + ')');
  print_directory_md_i := print_directory_md_i + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  sample := ['data_structures/linked_list.py', 'data_structures/binary_tree.py', 'math/number_theory/prime_check.py', 'math/number_theory/greatest_common_divisor.ipynb'];
  print_directory_md(sample);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
