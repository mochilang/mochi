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
  tree: SuffixTree;
  pattern: string;
function makeSuffixTree(text: string): SuffixTree; forward;
function new_suffix_tree(text: string): SuffixTree; forward;
function search(tree: SuffixTree; pattern: string): boolean; forward;
procedure main(); forward;
function makeSuffixTree(text: string): SuffixTree;
begin
  Result.text := text;
end;
function new_suffix_tree(text: string): SuffixTree;
begin
  exit(makeSuffixTree(text));
end;
function search(tree: SuffixTree; pattern: string): boolean;
var
  search_n: integer;
  search_m: integer;
  search_i: integer;
begin
  search_n := Length(tree.text);
  search_m := Length(pattern);
  if search_m = 0 then begin
  exit(true);
end;
  if search_m > search_n then begin
  exit(false);
end;
  search_i := 0;
  while search_i <= (search_n - search_m) do begin
  if copy(tree.text, search_i+1, (search_i + search_m - (search_i))) = pattern then begin
  exit(true);
end;
  search_i := search_i + 1;
end;
  exit(false);
end;
procedure main();
var
  main_text: string;
  main_suffix_tree: SuffixTree;
  main_patterns: array of string;
  main_i: integer;
  main_pattern: string;
  main_found: boolean;
begin
  main_text := 'monkey banana';
  main_suffix_tree := new_suffix_tree(main_text);
  main_patterns := ['ana', 'ban', 'na', 'xyz', 'mon'];
  main_i := 0;
  while main_i < Length(main_patterns) do begin
  main_pattern := main_patterns[main_i];
  main_found := search(main_suffix_tree, main_pattern);
  writeln((('Pattern ''' + main_pattern) + ''' found: ') + LowerCase(BoolToStr(main_found, true)));
  main_i := main_i + 1;
end;
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
