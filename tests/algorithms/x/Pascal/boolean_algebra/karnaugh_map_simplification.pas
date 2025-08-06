{$mode objfpc}
program Main;
uses SysUtils, StrUtils;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  kmap: array of array of integer;
  board: IntArrayArray;
  row: IntArray;
  terms: StrArray;
function row_string(row: IntArray): string; forward;
procedure print_kmap(kmap: IntArrayArray); forward;
function join_terms(terms: StrArray): string; forward;
function simplify_kmap(board: IntArrayArray): string; forward;
function row_string(row: IntArray): string;
var
  row_string_s: string;
  row_string_i: integer;
begin
  row_string_s := '[';
  row_string_i := 0;
  while row_string_i < Length(row) do begin
  row_string_s := row_string_s + IntToStr(row[row_string_i]);
  if row_string_i < (Length(row) - 1) then begin
  row_string_s := row_string_s + ', ';
end;
  row_string_i := row_string_i + 1;
end;
  row_string_s := row_string_s + ']';
  exit(row_string_s);
end;
procedure print_kmap(kmap: IntArrayArray);
var
  print_kmap_i: integer;
begin
  print_kmap_i := 0;
  while print_kmap_i < Length(kmap) do begin
  writeln(row_string(kmap[print_kmap_i]));
  print_kmap_i := print_kmap_i + 1;
end;
end;
function join_terms(terms: StrArray): string;
var
  join_terms_res: string;
  join_terms_i: integer;
begin
  if Length(terms) = 0 then begin
  exit('');
end;
  join_terms_res := terms[0];
  join_terms_i := 1;
  while join_terms_i < Length(terms) do begin
  join_terms_res := (join_terms_res + ' + ') + terms[join_terms_i];
  join_terms_i := join_terms_i + 1;
end;
  exit(join_terms_res);
end;
function simplify_kmap(board: IntArrayArray): string;
var
  simplify_kmap_terms: array of string;
  simplify_kmap_a: integer;
  simplify_kmap_row: array of integer;
  simplify_kmap_b: integer;
  simplify_kmap_item: integer;
  simplify_kmap_term: string;
  simplify_kmap_expr: string;
begin
  simplify_kmap_terms := [];
  simplify_kmap_a := 0;
  while simplify_kmap_a < Length(board) do begin
  simplify_kmap_row := board[simplify_kmap_a];
  simplify_kmap_b := 0;
  while simplify_kmap_b < Length(simplify_kmap_row) do begin
  simplify_kmap_item := simplify_kmap_row[simplify_kmap_b];
  if simplify_kmap_item <> 0 then begin
  simplify_kmap_term := IfThen(simplify_kmap_a <> 0, 'A', 'A''') + IfThen(simplify_kmap_b <> 0, 'B', 'B''');
  simplify_kmap_terms := concat(simplify_kmap_terms, StrArray([simplify_kmap_term]));
end;
  simplify_kmap_b := simplify_kmap_b + 1;
end;
  simplify_kmap_a := simplify_kmap_a + 1;
end;
  simplify_kmap_expr := join_terms(simplify_kmap_terms);
  exit(simplify_kmap_expr);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  kmap := [[0, 1], [1, 1]];
  print_kmap(kmap);
  writeln('Simplified Expression:');
  writeln(simplify_kmap(kmap));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
