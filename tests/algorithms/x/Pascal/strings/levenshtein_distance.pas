{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type IntArray = array of integer;
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
function range_list(range_list_n: integer): IntArray; forward;
function min3(min3_a: integer; min3_b: integer; min3_c: integer): integer; forward;
function levenshtein_distance(levenshtein_distance_first_word: string; levenshtein_distance_second_word: string): integer; forward;
function levenshtein_distance_optimized(levenshtein_distance_optimized_first_word: string; levenshtein_distance_optimized_second_word: string): integer; forward;
procedure main(); forward;
function range_list(range_list_n: integer): IntArray;
var
  range_list_lst: array of integer;
  range_list_i: integer;
begin
  range_list_lst := [];
  range_list_i := 0;
  while range_list_i < range_list_n do begin
  range_list_lst := concat(range_list_lst, [range_list_i]);
  range_list_i := range_list_i + 1;
end;
  exit(range_list_lst);
end;
function min3(min3_a: integer; min3_b: integer; min3_c: integer): integer;
var
  min3_m: integer;
begin
  min3_m := min3_a;
  if min3_b < min3_m then begin
  min3_m := min3_b;
end;
  if min3_c < min3_m then begin
  min3_m := min3_c;
end;
  exit(min3_m);
end;
function levenshtein_distance(levenshtein_distance_first_word: string; levenshtein_distance_second_word: string): integer;
var
  levenshtein_distance_previous_row: array of integer;
  levenshtein_distance_i: integer;
  levenshtein_distance_c1: string;
  levenshtein_distance_current_row: array of integer;
  levenshtein_distance_j: integer;
  levenshtein_distance_c2: string;
  levenshtein_distance_insertions: integer;
  levenshtein_distance_deletions: integer;
  levenshtein_distance_substitutions: integer;
  levenshtein_distance_min_val: integer;
begin
  if Length(levenshtein_distance_first_word) < Length(levenshtein_distance_second_word) then begin
  exit(levenshtein_distance(levenshtein_distance_second_word, levenshtein_distance_first_word));
end;
  if Length(levenshtein_distance_second_word) = 0 then begin
  exit(Length(levenshtein_distance_first_word));
end;
  levenshtein_distance_previous_row := range_list(Length(levenshtein_distance_second_word) + 1);
  levenshtein_distance_i := 0;
  while levenshtein_distance_i < Length(levenshtein_distance_first_word) do begin
  levenshtein_distance_c1 := levenshtein_distance_first_word[levenshtein_distance_i+1];
  levenshtein_distance_current_row := [];
  levenshtein_distance_current_row := concat(levenshtein_distance_current_row, [levenshtein_distance_i + 1]);
  levenshtein_distance_j := 0;
  while levenshtein_distance_j < Length(levenshtein_distance_second_word) do begin
  levenshtein_distance_c2 := levenshtein_distance_second_word[levenshtein_distance_j+1];
  levenshtein_distance_insertions := levenshtein_distance_previous_row[levenshtein_distance_j + 1] + 1;
  levenshtein_distance_deletions := levenshtein_distance_current_row[levenshtein_distance_j] + 1;
  levenshtein_distance_substitutions := levenshtein_distance_previous_row[levenshtein_distance_j] + IfThen(levenshtein_distance_c1 = levenshtein_distance_c2, 0, 1);
  levenshtein_distance_min_val := min3(levenshtein_distance_insertions, levenshtein_distance_deletions, levenshtein_distance_substitutions);
  levenshtein_distance_current_row := concat(levenshtein_distance_current_row, [levenshtein_distance_min_val]);
  levenshtein_distance_j := levenshtein_distance_j + 1;
end;
  levenshtein_distance_previous_row := levenshtein_distance_current_row;
  levenshtein_distance_i := levenshtein_distance_i + 1;
end;
  exit(levenshtein_distance_previous_row[Length(levenshtein_distance_previous_row) - 1]);
end;
function levenshtein_distance_optimized(levenshtein_distance_optimized_first_word: string; levenshtein_distance_optimized_second_word: string): integer;
var
  levenshtein_distance_optimized_previous_row: array of integer;
  levenshtein_distance_optimized_i: integer;
  levenshtein_distance_optimized_c1: string;
  levenshtein_distance_optimized_current_row: array of integer;
  levenshtein_distance_optimized_k: integer;
  levenshtein_distance_optimized_j: integer;
  levenshtein_distance_optimized_c2: string;
  levenshtein_distance_optimized_insertions: integer;
  levenshtein_distance_optimized_deletions: integer;
  levenshtein_distance_optimized_substitutions: integer;
  levenshtein_distance_optimized_min_val: integer;
begin
  if Length(levenshtein_distance_optimized_first_word) < Length(levenshtein_distance_optimized_second_word) then begin
  exit(levenshtein_distance_optimized(levenshtein_distance_optimized_second_word, levenshtein_distance_optimized_first_word));
end;
  if Length(levenshtein_distance_optimized_second_word) = 0 then begin
  exit(Length(levenshtein_distance_optimized_first_word));
end;
  levenshtein_distance_optimized_previous_row := range_list(Length(levenshtein_distance_optimized_second_word) + 1);
  levenshtein_distance_optimized_i := 0;
  while levenshtein_distance_optimized_i < Length(levenshtein_distance_optimized_first_word) do begin
  levenshtein_distance_optimized_c1 := levenshtein_distance_optimized_first_word[levenshtein_distance_optimized_i+1];
  levenshtein_distance_optimized_current_row := [];
  levenshtein_distance_optimized_current_row := concat(levenshtein_distance_optimized_current_row, [levenshtein_distance_optimized_i + 1]);
  levenshtein_distance_optimized_k := 0;
  while levenshtein_distance_optimized_k < Length(levenshtein_distance_optimized_second_word) do begin
  levenshtein_distance_optimized_current_row := concat(levenshtein_distance_optimized_current_row, [0]);
  levenshtein_distance_optimized_k := levenshtein_distance_optimized_k + 1;
end;
  levenshtein_distance_optimized_j := 0;
  while levenshtein_distance_optimized_j < Length(levenshtein_distance_optimized_second_word) do begin
  levenshtein_distance_optimized_c2 := levenshtein_distance_optimized_second_word[levenshtein_distance_optimized_j+1];
  levenshtein_distance_optimized_insertions := levenshtein_distance_optimized_previous_row[levenshtein_distance_optimized_j + 1] + 1;
  levenshtein_distance_optimized_deletions := levenshtein_distance_optimized_current_row[levenshtein_distance_optimized_j] + 1;
  levenshtein_distance_optimized_substitutions := levenshtein_distance_optimized_previous_row[levenshtein_distance_optimized_j] + IfThen(levenshtein_distance_optimized_c1 = levenshtein_distance_optimized_c2, 0, 1);
  levenshtein_distance_optimized_min_val := min3(levenshtein_distance_optimized_insertions, levenshtein_distance_optimized_deletions, levenshtein_distance_optimized_substitutions);
  levenshtein_distance_optimized_current_row[levenshtein_distance_optimized_j + 1] := levenshtein_distance_optimized_min_val;
  levenshtein_distance_optimized_j := levenshtein_distance_optimized_j + 1;
end;
  levenshtein_distance_optimized_previous_row := levenshtein_distance_optimized_current_row;
  levenshtein_distance_optimized_i := levenshtein_distance_optimized_i + 1;
end;
  exit(levenshtein_distance_optimized_previous_row[Length(levenshtein_distance_optimized_previous_row) - 1]);
end;
procedure main();
var
  main_a: string;
  main_b: string;
begin
  main_a := 'kitten';
  main_b := 'sitting';
  writeln(IntToStr(levenshtein_distance(main_a, main_b)));
  writeln(IntToStr(levenshtein_distance_optimized(main_a, main_b)));
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
