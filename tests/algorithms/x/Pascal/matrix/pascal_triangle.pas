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
  triangle: IntArrayArray;
  num_rows: integer;
  row_idx: integer;
  total_rows: integer;
  current_row_idx: integer;
  row: IntArray;
function populate_current_row(triangle: IntArrayArray; current_row_idx: integer): IntArray; forward;
function generate_pascal_triangle(num_rows: integer): IntArrayArray; forward;
function row_to_string(row: IntArray; total_rows: integer; row_idx: integer): string; forward;
procedure print_pascal_triangle(num_rows: integer); forward;
procedure main(); forward;
function populate_current_row(triangle: IntArrayArray; current_row_idx: integer): IntArray;
var
  populate_current_row_row: array of integer;
  populate_current_row_i: integer;
  populate_current_row_left: integer;
  populate_current_row_right: integer;
begin
  populate_current_row_row := [];
  populate_current_row_i := 0;
  while populate_current_row_i <= current_row_idx do begin
  if (populate_current_row_i = 0) or (populate_current_row_i = current_row_idx) then begin
  populate_current_row_row := concat(populate_current_row_row, IntArray([1]));
end else begin
  populate_current_row_left := triangle[current_row_idx - 1][populate_current_row_i - 1];
  populate_current_row_right := triangle[current_row_idx - 1][populate_current_row_i];
  populate_current_row_row := concat(populate_current_row_row, IntArray([populate_current_row_left + populate_current_row_right]));
end;
  populate_current_row_i := populate_current_row_i + 1;
end;
  exit(populate_current_row_row);
end;
function generate_pascal_triangle(num_rows: integer): IntArrayArray;
var
  generate_pascal_triangle_triangle: array of IntArray;
  generate_pascal_triangle_row_idx: integer;
  generate_pascal_triangle_row: IntArray;
begin
  if num_rows <= 0 then begin
  exit([]);
end;
  generate_pascal_triangle_triangle := [];
  generate_pascal_triangle_row_idx := 0;
  while generate_pascal_triangle_row_idx < num_rows do begin
  generate_pascal_triangle_row := populate_current_row(generate_pascal_triangle_triangle, generate_pascal_triangle_row_idx);
  generate_pascal_triangle_triangle := concat(generate_pascal_triangle_triangle, [generate_pascal_triangle_row]);
  generate_pascal_triangle_row_idx := generate_pascal_triangle_row_idx + 1;
end;
  exit(generate_pascal_triangle_triangle);
end;
function row_to_string(row: IntArray; total_rows: integer; row_idx: integer): string;
var
  row_to_string_line: string;
  row_to_string_spaces: integer;
  row_to_string_s: integer;
  row_to_string_c: integer;
begin
  row_to_string_line := '';
  row_to_string_spaces := (total_rows - row_idx) - 1;
  row_to_string_s := 0;
  while row_to_string_s < row_to_string_spaces do begin
  row_to_string_line := row_to_string_line + ' ';
  row_to_string_s := row_to_string_s + 1;
end;
  row_to_string_c := 0;
  while row_to_string_c <= row_idx do begin
  row_to_string_line := row_to_string_line + IntToStr(row[row_to_string_c]);
  if row_to_string_c <> row_idx then begin
  row_to_string_line := row_to_string_line + ' ';
end;
  row_to_string_c := row_to_string_c + 1;
end;
  exit(row_to_string_line);
end;
procedure print_pascal_triangle(num_rows: integer);
var
  print_pascal_triangle_triangle: IntArrayArray;
  print_pascal_triangle_r: integer;
  print_pascal_triangle_line: string;
begin
  print_pascal_triangle_triangle := generate_pascal_triangle(num_rows);
  print_pascal_triangle_r := 0;
  while print_pascal_triangle_r < num_rows do begin
  print_pascal_triangle_line := row_to_string(print_pascal_triangle_triangle[print_pascal_triangle_r], num_rows, print_pascal_triangle_r);
  writeln(print_pascal_triangle_line);
  print_pascal_triangle_r := print_pascal_triangle_r + 1;
end;
end;
procedure main();
begin
  print_pascal_triangle(5);
  writeln(list_list_int_to_str(generate_pascal_triangle(5)));
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
