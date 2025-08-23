{$mode objfpc}{$modeswitch nestedprocvars}
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
function split_ws(split_ws_s: string): StrArray; forward;
function contains(contains_xs: StrArray; contains_x: string): boolean; forward;
function unique(unique_xs: StrArray): StrArray; forward;
function insertion_sort(insertion_sort_arr: StrArray): StrArray; forward;
function join_with_space(join_with_space_xs: StrArray): string; forward;
function remove_duplicates(remove_duplicates_sentence: string): string; forward;
function split_ws(split_ws_s: string): StrArray;
var
  split_ws_res: array of string;
  split_ws_word: string;
  split_ws_i: integer;
  split_ws_ch: string;
begin
  split_ws_res := [];
  split_ws_word := '';
  split_ws_i := 0;
  while split_ws_i < Length(split_ws_s) do begin
  split_ws_ch := copy(split_ws_s, split_ws_i+1, (split_ws_i + 1 - (split_ws_i)));
  if split_ws_ch = ' ' then begin
  if split_ws_word <> '' then begin
  split_ws_res := concat(split_ws_res, StrArray([split_ws_word]));
  split_ws_word := '';
end;
end else begin
  split_ws_word := split_ws_word + split_ws_ch;
end;
  split_ws_i := split_ws_i + 1;
end;
  if split_ws_word <> '' then begin
  split_ws_res := concat(split_ws_res, StrArray([split_ws_word]));
end;
  exit(split_ws_res);
end;
function contains(contains_xs: StrArray; contains_x: string): boolean;
var
  contains_i: integer;
begin
  contains_i := 0;
  while contains_i < Length(contains_xs) do begin
  if contains_xs[contains_i] = contains_x then begin
  exit(true);
end;
  contains_i := contains_i + 1;
end;
  exit(false);
end;
function unique(unique_xs: StrArray): StrArray;
var
  unique_res: array of string;
  unique_i: integer;
  unique_w: string;
begin
  unique_res := [];
  unique_i := 0;
  while unique_i < Length(unique_xs) do begin
  unique_w := unique_xs[unique_i];
  if not contains(unique_res, unique_w) then begin
  unique_res := concat(unique_res, StrArray([unique_w]));
end;
  unique_i := unique_i + 1;
end;
  exit(unique_res);
end;
function insertion_sort(insertion_sort_arr: StrArray): StrArray;
var
  insertion_sort_a: array of string;
  insertion_sort_i: integer;
  insertion_sort_key: string;
  insertion_sort_j: integer;
begin
  insertion_sort_a := insertion_sort_arr;
  insertion_sort_i := 1;
  while insertion_sort_i < Length(insertion_sort_a) do begin
  insertion_sort_key := insertion_sort_a[insertion_sort_i];
  insertion_sort_j := insertion_sort_i - 1;
  while (insertion_sort_j >= 0) and (insertion_sort_a[insertion_sort_j] > insertion_sort_key) do begin
  insertion_sort_a[insertion_sort_j + 1] := insertion_sort_a[insertion_sort_j];
  insertion_sort_j := insertion_sort_j - 1;
end;
  insertion_sort_a[insertion_sort_j + 1] := insertion_sort_key;
  insertion_sort_i := insertion_sort_i + 1;
end;
  exit(insertion_sort_a);
end;
function join_with_space(join_with_space_xs: StrArray): string;
var
  join_with_space_s: string;
  join_with_space_i: integer;
begin
  join_with_space_s := '';
  join_with_space_i := 0;
  while join_with_space_i < Length(join_with_space_xs) do begin
  if join_with_space_i > 0 then begin
  join_with_space_s := join_with_space_s + ' ';
end;
  join_with_space_s := join_with_space_s + join_with_space_xs[join_with_space_i];
  join_with_space_i := join_with_space_i + 1;
end;
  exit(join_with_space_s);
end;
function remove_duplicates(remove_duplicates_sentence: string): string;
var
  remove_duplicates_words: StrArray;
  remove_duplicates_uniq: StrArray;
  remove_duplicates_sorted_words: StrArray;
begin
  remove_duplicates_words := split_ws(remove_duplicates_sentence);
  remove_duplicates_uniq := unique(remove_duplicates_words);
  remove_duplicates_sorted_words := insertion_sort(remove_duplicates_uniq);
  exit(join_with_space(remove_duplicates_sorted_words));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(remove_duplicates('Python is great and Java is also great'));
  writeln(remove_duplicates('Python   is      great and Java is also great'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
