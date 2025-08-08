{$mode objfpc}
program Main;
uses SysUtils;
type SuffixTree = record
  text: string;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  text: string;
  st: SuffixTree;
  patterns_exist: array of string;
  i: integer;
  patterns_none: array of string;
  substrings: array of string;
  pattern: string;
function makeSuffixTree(text: string): SuffixTree; forward;
function suffix_tree_new(text: string): SuffixTree; forward;
function suffix_tree_search(st: SuffixTree; pattern: string): boolean; forward;
function makeSuffixTree(text: string): SuffixTree;
begin
  Result.text := text;
end;
function suffix_tree_new(text: string): SuffixTree;
begin
  exit(makeSuffixTree(text));
end;
function suffix_tree_search(st: SuffixTree; pattern: string): boolean;
var
  suffix_tree_search_i: integer;
  suffix_tree_search_n: integer;
  suffix_tree_search_m: integer;
  suffix_tree_search_j: integer;
  suffix_tree_search_found: boolean;
begin
  if Length(pattern) = 0 then begin
  exit(true);
end;
  suffix_tree_search_i := 0;
  suffix_tree_search_n := Length(st.text);
  suffix_tree_search_m := Length(pattern);
  while suffix_tree_search_i <= (suffix_tree_search_n - suffix_tree_search_m) do begin
  suffix_tree_search_j := 0;
  suffix_tree_search_found := true;
  while suffix_tree_search_j < suffix_tree_search_m do begin
  if st.text[suffix_tree_search_i + suffix_tree_search_j+1] <> pattern[suffix_tree_search_j+1] then begin
  suffix_tree_search_found := false;
  break;
end;
  suffix_tree_search_j := suffix_tree_search_j + 1;
end;
  if suffix_tree_search_found then begin
  exit(true);
end;
  suffix_tree_search_i := suffix_tree_search_i + 1;
end;
  exit(false);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  text := 'banana';
  st := suffix_tree_new(text);
  patterns_exist := ['ana', 'ban', 'na'];
  i := 0;
  while i < Length(patterns_exist) do begin
  writeln(LowerCase(BoolToStr(suffix_tree_search(st, patterns_exist[i]), true)));
  i := i + 1;
end;
  patterns_none := ['xyz', 'apple', 'cat'];
  i := 0;
  while i < Length(patterns_none) do begin
  writeln(LowerCase(BoolToStr(suffix_tree_search(st, patterns_none[i]), true)));
  i := i + 1;
end;
  writeln(LowerCase(BoolToStr(suffix_tree_search(st, ''), true)));
  writeln(LowerCase(BoolToStr(suffix_tree_search(st, text), true)));
  substrings := ['ban', 'ana', 'a', 'na'];
  i := 0;
  while i < Length(substrings) do begin
  writeln(LowerCase(BoolToStr(suffix_tree_search(st, substrings[i]), true)));
  i := i + 1;
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
