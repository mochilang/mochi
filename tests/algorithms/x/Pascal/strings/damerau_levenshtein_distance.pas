{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
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
function damerau_levenshtein_distance(damerau_levenshtein_distance_first_string: string; damerau_levenshtein_distance_second_string: string): integer; forward;
function damerau_levenshtein_distance(damerau_levenshtein_distance_first_string: string; damerau_levenshtein_distance_second_string: string): integer;
var
  damerau_levenshtein_distance_len1: integer;
  damerau_levenshtein_distance_len2: integer;
  damerau_levenshtein_distance_dp_matrix: array of IntArray;
  damerau_levenshtein_distance__: int64;
  damerau_levenshtein_distance_row: array of integer;
  damerau_levenshtein_distance__2: int64;
  damerau_levenshtein_distance_i: int64;
  damerau_levenshtein_distance_first_row: array of integer;
  damerau_levenshtein_distance_j: int64;
  damerau_levenshtein_distance_first_char: string;
  damerau_levenshtein_distance_second_char: string;
  damerau_levenshtein_distance_cost: integer;
  damerau_levenshtein_distance_value: integer;
  damerau_levenshtein_distance_insertion: integer;
  damerau_levenshtein_distance_substitution: integer;
  damerau_levenshtein_distance_transposition: integer;
begin
  damerau_levenshtein_distance_len1 := Length(damerau_levenshtein_distance_first_string);
  damerau_levenshtein_distance_len2 := Length(damerau_levenshtein_distance_second_string);
  damerau_levenshtein_distance_dp_matrix := [];
  for damerau_levenshtein_distance__ := 0 to (damerau_levenshtein_distance_len1 + 1 - 1) do begin
  damerau_levenshtein_distance_row := [];
  for damerau_levenshtein_distance__2 := 0 to (damerau_levenshtein_distance_len2 + 1 - 1) do begin
  damerau_levenshtein_distance_row := concat(damerau_levenshtein_distance_row, [0]);
end;
  damerau_levenshtein_distance_dp_matrix := concat(damerau_levenshtein_distance_dp_matrix, [damerau_levenshtein_distance_row]);
end;
  for damerau_levenshtein_distance_i := 0 to (damerau_levenshtein_distance_len1 + 1 - 1) do begin
  damerau_levenshtein_distance_row := damerau_levenshtein_distance_dp_matrix[damerau_levenshtein_distance_i];
  damerau_levenshtein_distance_row[0] := damerau_levenshtein_distance_i;
  damerau_levenshtein_distance_dp_matrix[damerau_levenshtein_distance_i] := damerau_levenshtein_distance_row;
end;
  damerau_levenshtein_distance_first_row := damerau_levenshtein_distance_dp_matrix[0];
  for damerau_levenshtein_distance_j := 0 to (damerau_levenshtein_distance_len2 + 1 - 1) do begin
  damerau_levenshtein_distance_first_row[damerau_levenshtein_distance_j] := damerau_levenshtein_distance_j;
end;
  damerau_levenshtein_distance_dp_matrix[0] := damerau_levenshtein_distance_first_row;
  for damerau_levenshtein_distance_i := 1 to (damerau_levenshtein_distance_len1 + 1 - 1) do begin
  damerau_levenshtein_distance_row := damerau_levenshtein_distance_dp_matrix[damerau_levenshtein_distance_i];
  damerau_levenshtein_distance_first_char := copy(damerau_levenshtein_distance_first_string, damerau_levenshtein_distance_i - 1+1, (damerau_levenshtein_distance_i - (damerau_levenshtein_distance_i - 1)));
  for damerau_levenshtein_distance_j := 1 to (damerau_levenshtein_distance_len2 + 1 - 1) do begin
  damerau_levenshtein_distance_second_char := copy(damerau_levenshtein_distance_second_string, damerau_levenshtein_distance_j - 1+1, (damerau_levenshtein_distance_j - (damerau_levenshtein_distance_j - 1)));
  if damerau_levenshtein_distance_first_char = damerau_levenshtein_distance_second_char then begin
  damerau_levenshtein_distance_cost := 0;
end else begin
  damerau_levenshtein_distance_cost := 1;
end;
  damerau_levenshtein_distance_value := damerau_levenshtein_distance_dp_matrix[damerau_levenshtein_distance_i - 1][damerau_levenshtein_distance_j] + 1;
  damerau_levenshtein_distance_insertion := damerau_levenshtein_distance_row[damerau_levenshtein_distance_j - 1] + 1;
  if damerau_levenshtein_distance_insertion < damerau_levenshtein_distance_value then begin
  damerau_levenshtein_distance_value := damerau_levenshtein_distance_insertion;
end;
  damerau_levenshtein_distance_substitution := damerau_levenshtein_distance_dp_matrix[damerau_levenshtein_distance_i - 1][damerau_levenshtein_distance_j - 1] + damerau_levenshtein_distance_cost;
  if damerau_levenshtein_distance_substitution < damerau_levenshtein_distance_value then begin
  damerau_levenshtein_distance_value := damerau_levenshtein_distance_substitution;
end;
  damerau_levenshtein_distance_row[damerau_levenshtein_distance_j] := damerau_levenshtein_distance_value;
  if (((damerau_levenshtein_distance_i > 1) and (damerau_levenshtein_distance_j > 1)) and (copy(damerau_levenshtein_distance_first_string, damerau_levenshtein_distance_i - 1+1, (damerau_levenshtein_distance_i - (damerau_levenshtein_distance_i - 1))) = copy(damerau_levenshtein_distance_second_string, damerau_levenshtein_distance_j - 2+1, (damerau_levenshtein_distance_j - 1 - (damerau_levenshtein_distance_j - 2))))) and (copy(damerau_levenshtein_distance_first_string, damerau_levenshtein_distance_i - 2+1, (damerau_levenshtein_distance_i - 1 - (damerau_levenshtein_distance_i - 2))) = copy(damerau_levenshtein_distance_second_string, damerau_levenshtein_distance_j - 1+1, (damerau_levenshtein_distance_j - (damerau_levenshtein_distance_j - 1)))) then begin
  damerau_levenshtein_distance_transposition := damerau_levenshtein_distance_dp_matrix[damerau_levenshtein_distance_i - 2][damerau_levenshtein_distance_j - 2] + damerau_levenshtein_distance_cost;
  if damerau_levenshtein_distance_transposition < damerau_levenshtein_distance_row[damerau_levenshtein_distance_j] then begin
  damerau_levenshtein_distance_row[damerau_levenshtein_distance_j] := damerau_levenshtein_distance_transposition;
end;
end;
end;
  damerau_levenshtein_distance_dp_matrix[damerau_levenshtein_distance_i] := damerau_levenshtein_distance_row;
end;
  exit(damerau_levenshtein_distance_dp_matrix[damerau_levenshtein_distance_len1][damerau_levenshtein_distance_len2]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(damerau_levenshtein_distance('cat', 'cut')));
  writeln(IntToStr(damerau_levenshtein_distance('kitten', 'sitting')));
  writeln(IntToStr(damerau_levenshtein_distance('hello', 'world')));
  writeln(IntToStr(damerau_levenshtein_distance('book', 'back')));
  writeln(IntToStr(damerau_levenshtein_distance('container', 'containment')));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
