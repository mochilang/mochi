{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
type StrArray = array of string;
var _dataDir: string = '/workspace/mochi/tests/github/TheAlgorithms/Mochi/strings';
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
function _read_file(path: string): string;
var f: Text; line, out, pth: string;
begin
  pth := path;
  if (_dataDir <> '') and (not FileExists(pth)) then pth := _dataDir + '/' + path;
  out := '';
  if FileExists(pth) then begin
    Assign(f, pth);
    Reset(f);
    while not EOF(f) do begin
      ReadLn(f, line);
      if out <> '' then out := out + #10;
      out := out + line;
    end;
    Close(f);
  end;
  _read_file := out;
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
  word_by_signature: specialize TFPGMap<string, StrArray>;
function split(split_s: string; split_sep: string): StrArray; forward;
function insertion_sort(insertion_sort_arr: StrArray): StrArray; forward;
function sort_chars(sort_chars_word: string): string; forward;
function unique_sorted(unique_sorted_words: StrArray): StrArray; forward;
procedure build_map(build_map_words: StrArray); forward;
function anagram(anagram_my_word: string): StrArray; forward;
procedure main(); forward;
function split(split_s: string; split_sep: string): StrArray;
var
  split_res: array of string;
  split_current: string;
  split_i: integer;
  split_ch: string;
begin
  split_res := [];
  split_current := '';
  split_i := 0;
  while split_i < Length(split_s) do begin
  split_ch := copy(split_s, split_i+1, (split_i + 1 - (split_i)));
  if split_ch = split_sep then begin
  split_res := concat(split_res, StrArray([split_current]));
  split_current := '';
end else begin
  split_current := split_current + split_ch;
end;
  split_i := split_i + 1;
end;
  split_res := concat(split_res, StrArray([split_current]));
  exit(split_res);
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
function sort_chars(sort_chars_word: string): string;
var
  sort_chars_chars: array of string;
  sort_chars_i: integer;
  sort_chars_res: string;
begin
  sort_chars_chars := [];
  sort_chars_i := 0;
  while sort_chars_i < Length(sort_chars_word) do begin
  sort_chars_chars := concat(sort_chars_chars, StrArray([copy(sort_chars_word, sort_chars_i+1, (sort_chars_i + 1 - (sort_chars_i)))]));
  sort_chars_i := sort_chars_i + 1;
end;
  sort_chars_chars := insertion_sort(sort_chars_chars);
  sort_chars_res := '';
  sort_chars_i := 0;
  while sort_chars_i < Length(sort_chars_chars) do begin
  sort_chars_res := sort_chars_res + sort_chars_chars[sort_chars_i];
  sort_chars_i := sort_chars_i + 1;
end;
  exit(sort_chars_res);
end;
function unique_sorted(unique_sorted_words: StrArray): StrArray;
var
  unique_sorted_seen: specialize TFPGMap<string, boolean>;
  unique_sorted_res: array of string;
  unique_sorted_w: string;
begin
  unique_sorted_seen := specialize TFPGMap<string, boolean>.Create();
  unique_sorted_res := [];
  for unique_sorted_w in unique_sorted_words do begin
  if (unique_sorted_w <> '') and not(unique_sorted_seen.IndexOf(unique_sorted_w) <> -1) then begin
  unique_sorted_res := concat(unique_sorted_res, StrArray([unique_sorted_w]));
  unique_sorted_seen[unique_sorted_w] := true;
end;
end;
  unique_sorted_res := insertion_sort(unique_sorted_res);
  exit(unique_sorted_res);
end;
procedure build_map(build_map_words: StrArray);
var
  build_map_w: string;
  build_map_sig: string;
  build_map_arr: array of string;
begin
  for build_map_w in build_map_words do begin
  build_map_sig := sort_chars(build_map_w);
  build_map_arr := [];
  if word_by_signature.IndexOf(build_map_sig) <> -1 then begin
  build_map_arr := word_by_signature[build_map_sig];
end;
  build_map_arr := concat(build_map_arr, StrArray([build_map_w]));
  word_by_signature[build_map_sig] := build_map_arr;
end;
end;
function anagram(anagram_my_word: string): StrArray;
var
  anagram_sig: string;
begin
  anagram_sig := sort_chars(anagram_my_word);
  if word_by_signature.IndexOf(anagram_sig) <> -1 then begin
  exit(word_by_signature[anagram_sig]);
end;
  exit([]);
end;
procedure main();
var
  main_text: string;
  main_lines: StrArray;
  main_words: StrArray;
  main_w: string;
  main_anas: StrArray;
  main_line: string;
  main_i: integer;
begin
  main_text := _read_file('words.txt');
  main_lines := split(main_text, #10);
  main_words := unique_sorted(main_lines);
  build_map(main_words);
  for main_w in main_words do begin
  main_anas := anagram(main_w);
  if Length(main_anas) > 1 then begin
  main_line := main_w + ':';
  main_i := 0;
  while main_i < Length(main_anas) do begin
  if main_i > 0 then begin
  main_line := main_line + ',';
end;
  main_line := main_line + main_anas[main_i];
  main_i := main_i + 1;
end;
  writeln(main_line);
end;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  word_by_signature := specialize TFPGMap<string, StrArray>.Create();
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
