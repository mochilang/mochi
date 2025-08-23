{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type StrArray = array of string;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
type StrArrayArray = array of StrArray;
type TransformTables = record
  costs: array of IntArray;
  ops: array of StrArray;
end;
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
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function makeTransformTables(costs: IntArrayArray; ops: StrArrayArray): TransformTables; forward;
function string_to_chars(string_to_chars_s: string): StrArray; forward;
function join_chars(join_chars_chars: StrArray): string; forward;
function insert_at(insert_at_chars: StrArray; insert_at_index: integer; insert_at_ch: string): StrArray; forward;
function remove_at(remove_at_chars: StrArray; remove_at_index: integer): StrArray; forward;
function make_matrix_int(make_matrix_int_rows: integer; make_matrix_int_cols: integer; make_matrix_int_init: integer): IntArrayArray; forward;
function make_matrix_string(make_matrix_string_rows: integer; make_matrix_string_cols: integer; make_matrix_string_init: string): StrArrayArray; forward;
function compute_transform_tables(compute_transform_tables_source_string: string; compute_transform_tables_destination_string: string; compute_transform_tables_copy_cost: integer; compute_transform_tables_replace_cost: integer; compute_transform_tables_delete_cost: integer; compute_transform_tables_insert_cost: integer): TransformTables; forward;
function assemble_transformation(assemble_transformation_ops: StrArrayArray; assemble_transformation_i: integer; assemble_transformation_j: integer): StrArray; forward;
procedure main(); forward;
function makeTransformTables(costs: IntArrayArray; ops: StrArrayArray): TransformTables;
begin
  Result.costs := costs;
  Result.ops := ops;
end;
function string_to_chars(string_to_chars_s: string): StrArray;
var
  string_to_chars_chars: array of string;
  string_to_chars_i: integer;
begin
  string_to_chars_chars := [];
  string_to_chars_i := 0;
  while string_to_chars_i < Length(string_to_chars_s) do begin
  string_to_chars_chars := concat(string_to_chars_chars, StrArray([copy(string_to_chars_s, string_to_chars_i+1, (string_to_chars_i + 1 - (string_to_chars_i)))]));
  string_to_chars_i := string_to_chars_i + 1;
end;
  exit(string_to_chars_chars);
end;
function join_chars(join_chars_chars: StrArray): string;
var
  join_chars_res: string;
  join_chars_i: integer;
begin
  join_chars_res := '';
  join_chars_i := 0;
  while join_chars_i < Length(join_chars_chars) do begin
  join_chars_res := join_chars_res + join_chars_chars[join_chars_i];
  join_chars_i := join_chars_i + 1;
end;
  exit(join_chars_res);
end;
function insert_at(insert_at_chars: StrArray; insert_at_index: integer; insert_at_ch: string): StrArray;
var
  insert_at_res: array of string;
  insert_at_i: integer;
begin
  insert_at_res := [];
  insert_at_i := 0;
  while insert_at_i < insert_at_index do begin
  insert_at_res := concat(insert_at_res, StrArray([insert_at_chars[insert_at_i]]));
  insert_at_i := insert_at_i + 1;
end;
  insert_at_res := concat(insert_at_res, StrArray([insert_at_ch]));
  while insert_at_i < Length(insert_at_chars) do begin
  insert_at_res := concat(insert_at_res, StrArray([insert_at_chars[insert_at_i]]));
  insert_at_i := insert_at_i + 1;
end;
  exit(insert_at_res);
end;
function remove_at(remove_at_chars: StrArray; remove_at_index: integer): StrArray;
var
  remove_at_res: array of string;
  remove_at_i: integer;
begin
  remove_at_res := [];
  remove_at_i := 0;
  while remove_at_i < Length(remove_at_chars) do begin
  if remove_at_i <> remove_at_index then begin
  remove_at_res := concat(remove_at_res, StrArray([remove_at_chars[remove_at_i]]));
end;
  remove_at_i := remove_at_i + 1;
end;
  exit(remove_at_res);
end;
function make_matrix_int(make_matrix_int_rows: integer; make_matrix_int_cols: integer; make_matrix_int_init: integer): IntArrayArray;
var
  make_matrix_int_matrix: array of IntArray;
  make_matrix_int__: int64;
  make_matrix_int_row: array of integer;
  make_matrix_int__2: int64;
begin
  make_matrix_int_matrix := [];
  for make_matrix_int__ := 0 to (make_matrix_int_rows - 1) do begin
  make_matrix_int_row := [];
  for make_matrix_int__2 := 0 to (make_matrix_int_cols - 1) do begin
  make_matrix_int_row := concat(make_matrix_int_row, [make_matrix_int_init]);
end;
  make_matrix_int_matrix := concat(make_matrix_int_matrix, [make_matrix_int_row]);
end;
  exit(make_matrix_int_matrix);
end;
function make_matrix_string(make_matrix_string_rows: integer; make_matrix_string_cols: integer; make_matrix_string_init: string): StrArrayArray;
var
  make_matrix_string_matrix: array of StrArray;
  make_matrix_string__: int64;
  make_matrix_string_row: array of string;
  make_matrix_string__2: int64;
begin
  make_matrix_string_matrix := [];
  for make_matrix_string__ := 0 to (make_matrix_string_rows - 1) do begin
  make_matrix_string_row := [];
  for make_matrix_string__2 := 0 to (make_matrix_string_cols - 1) do begin
  make_matrix_string_row := concat(make_matrix_string_row, StrArray([make_matrix_string_init]));
end;
  make_matrix_string_matrix := concat(make_matrix_string_matrix, [make_matrix_string_row]);
end;
  exit(make_matrix_string_matrix);
end;
function compute_transform_tables(compute_transform_tables_source_string: string; compute_transform_tables_destination_string: string; compute_transform_tables_copy_cost: integer; compute_transform_tables_replace_cost: integer; compute_transform_tables_delete_cost: integer; compute_transform_tables_insert_cost: integer): TransformTables;
var
  compute_transform_tables_source_seq: StrArray;
  compute_transform_tables_dest_seq: StrArray;
  compute_transform_tables_m: integer;
  compute_transform_tables_n: integer;
  compute_transform_tables_costs: IntArrayArray;
  compute_transform_tables_ops: StrArrayArray;
  compute_transform_tables_i: integer;
  compute_transform_tables_j: integer;
begin
  compute_transform_tables_source_seq := string_to_chars(compute_transform_tables_source_string);
  compute_transform_tables_dest_seq := string_to_chars(compute_transform_tables_destination_string);
  compute_transform_tables_m := Length(compute_transform_tables_source_seq);
  compute_transform_tables_n := Length(compute_transform_tables_dest_seq);
  compute_transform_tables_costs := make_matrix_int(compute_transform_tables_m + 1, compute_transform_tables_n + 1, 0);
  compute_transform_tables_ops := make_matrix_string(compute_transform_tables_m + 1, compute_transform_tables_n + 1, '0');
  compute_transform_tables_i := 1;
  while compute_transform_tables_i <= compute_transform_tables_m do begin
  compute_transform_tables_costs[compute_transform_tables_i][0] := compute_transform_tables_i * compute_transform_tables_delete_cost;
  compute_transform_tables_ops[compute_transform_tables_i][0] := 'D' + compute_transform_tables_source_seq[compute_transform_tables_i - 1];
  compute_transform_tables_i := compute_transform_tables_i + 1;
end;
  compute_transform_tables_j := 1;
  while compute_transform_tables_j <= compute_transform_tables_n do begin
  compute_transform_tables_costs[0][compute_transform_tables_j] := compute_transform_tables_j * compute_transform_tables_insert_cost;
  compute_transform_tables_ops[0][compute_transform_tables_j] := 'I' + compute_transform_tables_dest_seq[compute_transform_tables_j - 1];
  compute_transform_tables_j := compute_transform_tables_j + 1;
end;
  compute_transform_tables_i := 1;
  while compute_transform_tables_i <= compute_transform_tables_m do begin
  compute_transform_tables_j := 1;
  while compute_transform_tables_j <= compute_transform_tables_n do begin
  if compute_transform_tables_source_seq[compute_transform_tables_i - 1] = compute_transform_tables_dest_seq[compute_transform_tables_j - 1] then begin
  compute_transform_tables_costs[compute_transform_tables_i][compute_transform_tables_j] := compute_transform_tables_costs[compute_transform_tables_i - 1][compute_transform_tables_j - 1] + compute_transform_tables_copy_cost;
  compute_transform_tables_ops[compute_transform_tables_i][compute_transform_tables_j] := 'C' + compute_transform_tables_source_seq[compute_transform_tables_i - 1];
end else begin
  compute_transform_tables_costs[compute_transform_tables_i][compute_transform_tables_j] := compute_transform_tables_costs[compute_transform_tables_i - 1][compute_transform_tables_j - 1] + compute_transform_tables_replace_cost;
  compute_transform_tables_ops[compute_transform_tables_i][compute_transform_tables_j] := ('R' + compute_transform_tables_source_seq[compute_transform_tables_i - 1]) + compute_transform_tables_dest_seq[compute_transform_tables_j - 1];
end;
  if (compute_transform_tables_costs[compute_transform_tables_i - 1][compute_transform_tables_j] + compute_transform_tables_delete_cost) < compute_transform_tables_costs[compute_transform_tables_i][compute_transform_tables_j] then begin
  compute_transform_tables_costs[compute_transform_tables_i][compute_transform_tables_j] := compute_transform_tables_costs[compute_transform_tables_i - 1][compute_transform_tables_j] + compute_transform_tables_delete_cost;
  compute_transform_tables_ops[compute_transform_tables_i][compute_transform_tables_j] := 'D' + compute_transform_tables_source_seq[compute_transform_tables_i - 1];
end;
  if (compute_transform_tables_costs[compute_transform_tables_i][compute_transform_tables_j - 1] + compute_transform_tables_insert_cost) < compute_transform_tables_costs[compute_transform_tables_i][compute_transform_tables_j] then begin
  compute_transform_tables_costs[compute_transform_tables_i][compute_transform_tables_j] := compute_transform_tables_costs[compute_transform_tables_i][compute_transform_tables_j - 1] + compute_transform_tables_insert_cost;
  compute_transform_tables_ops[compute_transform_tables_i][compute_transform_tables_j] := 'I' + compute_transform_tables_dest_seq[compute_transform_tables_j - 1];
end;
  compute_transform_tables_j := compute_transform_tables_j + 1;
end;
  compute_transform_tables_i := compute_transform_tables_i + 1;
end;
  exit(makeTransformTables(compute_transform_tables_costs, compute_transform_tables_ops));
end;
function assemble_transformation(assemble_transformation_ops: StrArrayArray; assemble_transformation_i: integer; assemble_transformation_j: integer): StrArray;
var
  assemble_transformation_op: string;
  assemble_transformation_kind: string;
  assemble_transformation_seq: StrArray;
  assemble_transformation_seq_31: StrArray;
  assemble_transformation_seq_32: StrArray;
begin
  if (assemble_transformation_i = 0) and (assemble_transformation_j = 0) then begin
  exit([]);
end;
  assemble_transformation_op := assemble_transformation_ops[assemble_transformation_i][assemble_transformation_j];
  assemble_transformation_kind := copy(assemble_transformation_op, 1, 1);
  if (assemble_transformation_kind = 'C') or (assemble_transformation_kind = 'R') then begin
  assemble_transformation_seq := assemble_transformation(assemble_transformation_ops, assemble_transformation_i - 1, assemble_transformation_j - 1);
  assemble_transformation_seq := concat(assemble_transformation_seq, StrArray([assemble_transformation_op]));
  exit(assemble_transformation_seq);
end else begin
  if assemble_transformation_kind = 'D' then begin
  assemble_transformation_seq_31 := assemble_transformation(assemble_transformation_ops, assemble_transformation_i - 1, assemble_transformation_j);
  assemble_transformation_seq_31 := concat(assemble_transformation_seq_31, StrArray([assemble_transformation_op]));
  exit(assemble_transformation_seq_31);
end else begin
  assemble_transformation_seq_32 := assemble_transformation(assemble_transformation_ops, assemble_transformation_i, assemble_transformation_j - 1);
  assemble_transformation_seq_32 := concat(assemble_transformation_seq_32, StrArray([assemble_transformation_op]));
  exit(assemble_transformation_seq_32);
end;
end;
end;
procedure main();
var
  main_copy_cost: integer;
  main_replace_cost: integer;
  main_delete_cost: integer;
  main_insert_cost: integer;
  main_src: string;
  main_dst: string;
  main_tables: TransformTables;
  main_operations: array of StrArray;
  main_m: integer;
  main_n: integer;
  main_sequence: StrArray;
  main_string_list: StrArray;
  main_idx: integer;
  main_cost: integer;
  main_k: integer;
  main_op: string;
  main_kind: string;
begin
  main_copy_cost := -1;
  main_replace_cost := 1;
  main_delete_cost := 2;
  main_insert_cost := 2;
  main_src := 'Python';
  main_dst := 'Algorithms';
  main_tables := compute_transform_tables(main_src, main_dst, main_copy_cost, main_replace_cost, main_delete_cost, main_insert_cost);
  main_operations := main_tables.ops;
  main_m := Length(main_operations);
  main_n := Length(main_operations[0]);
  main_sequence := assemble_transformation(main_operations, main_m - 1, main_n - 1);
  main_string_list := string_to_chars(main_src);
  main_idx := 0;
  main_cost := 0;
  main_k := 0;
  while main_k < Length(main_sequence) do begin
  writeln(join_chars(main_string_list));
  main_op := main_sequence[main_k];
  main_kind := copy(main_op, 1, 1);
  if main_kind = 'C' then begin
  main_cost := main_cost + main_copy_cost;
end else begin
  if main_kind = 'R' then begin
  main_string_list[main_idx] := copy(main_op, 3, 1);
  main_cost := main_cost + main_replace_cost;
end else begin
  if main_kind = 'D' then begin
  main_string_list := remove_at(main_string_list, main_idx);
  main_cost := main_cost + main_delete_cost;
end else begin
  main_string_list := insert_at(main_string_list, main_idx, copy(main_op, 2, 1));
  main_cost := main_cost + main_insert_cost;
end;
end;
end;
  main_idx := main_idx + 1;
  main_k := main_k + 1;
end;
  writeln(join_chars(main_string_list));
  writeln('Cost: ' + IntToStr(main_cost));
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
  writeln('');
end.
