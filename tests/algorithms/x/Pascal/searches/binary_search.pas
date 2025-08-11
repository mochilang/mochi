{$mode objfpc}
program Main;
uses SysUtils;
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
function min(xs: array of integer): integer;
var i, m: integer;
begin
  if Length(xs) = 0 then begin min := 0; exit; end;
  m := xs[0];
  for i := 1 to High(xs) do if xs[i] < m then m := xs[i];
  min := m;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  item: integer;
  arr: IntArray;
  sorted_collection: IntArray;
  left: integer;
  right: integer;
function is_sorted(arr: IntArray): boolean; forward;
function binary_search(sorted_collection: IntArray; item: integer): integer; forward;
function binary_search_by_recursion(sorted_collection: IntArray; item: integer; left: integer; right: integer): integer; forward;
function exponential_search(sorted_collection: IntArray; item: integer): integer; forward;
procedure main(); forward;
function is_sorted(arr: IntArray): boolean;
var
  is_sorted_i: integer;
begin
  is_sorted_i := 1;
  while is_sorted_i < Length(arr) do begin
  if arr[is_sorted_i - 1] > arr[is_sorted_i] then begin
  exit(false);
end;
  is_sorted_i := is_sorted_i + 1;
end;
  exit(true);
end;
function binary_search(sorted_collection: IntArray; item: integer): integer;
var
  binary_search_left: integer;
  binary_search_right: integer;
  binary_search_midpoint: integer;
  binary_search_current_item: integer;
begin
  if not is_sorted(sorted_collection) then begin
  exit(-1);
end;
  binary_search_left := 0;
  binary_search_right := Length(sorted_collection) - 1;
  while binary_search_left <= binary_search_right do begin
  binary_search_midpoint := binary_search_left + ((binary_search_right - binary_search_left) div 2);
  binary_search_current_item := sorted_collection[binary_search_midpoint];
  if binary_search_current_item = item then begin
  exit(binary_search_midpoint);
end;
  if item < binary_search_current_item then begin
  binary_search_right := binary_search_midpoint - 1;
end else begin
  binary_search_left := binary_search_midpoint + 1;
end;
end;
  exit(-1);
end;
function binary_search_by_recursion(sorted_collection: IntArray; item: integer; left: integer; right: integer): integer;
var
  binary_search_by_recursion_midpoint: integer;
begin
  if right < left then begin
  exit(-1);
end;
  binary_search_by_recursion_midpoint := left + ((right - left) div 2);
  if sorted_collection[binary_search_by_recursion_midpoint] = item then begin
  exit(binary_search_by_recursion_midpoint);
end;
  if sorted_collection[binary_search_by_recursion_midpoint] > item then begin
  exit(binary_search_by_recursion(sorted_collection, item, left, binary_search_by_recursion_midpoint - 1));
end;
  exit(binary_search_by_recursion(sorted_collection, item, binary_search_by_recursion_midpoint + 1, right));
end;
function exponential_search(sorted_collection: IntArray; item: integer): integer;
var
  exponential_search_bound: integer;
  exponential_search_left: integer;
  exponential_search_right: integer;
begin
  if not is_sorted(sorted_collection) then begin
  exit(-1);
end;
  if Length(sorted_collection) = 0 then begin
  exit(-1);
end;
  exponential_search_bound := 1;
  while (exponential_search_bound < Length(sorted_collection)) and (sorted_collection[exponential_search_bound] < item) do begin
  exponential_search_bound := exponential_search_bound * 2;
end;
  exponential_search_left := exponential_search_bound div 2;
  exponential_search_right := min([exponential_search_bound, Length(sorted_collection) - 1]);
  exit(binary_search_by_recursion(sorted_collection, item, exponential_search_left, exponential_search_right));
end;
procedure main();
var
  main_data: array of integer;
begin
  main_data := [0, 5, 7, 10, 15];
  writeln(IntToStr(binary_search(main_data, 0)));
  writeln(IntToStr(binary_search(main_data, 15)));
  writeln(IntToStr(binary_search(main_data, 5)));
  writeln(IntToStr(binary_search(main_data, 6)));
  writeln(IntToStr(binary_search_by_recursion(main_data, 0, 0, Length(main_data) - 1)));
  writeln(IntToStr(binary_search_by_recursion(main_data, 15, 0, Length(main_data) - 1)));
  writeln(IntToStr(binary_search_by_recursion(main_data, 5, 0, Length(main_data) - 1)));
  writeln(IntToStr(binary_search_by_recursion(main_data, 6, 0, Length(main_data) - 1)));
  writeln(IntToStr(exponential_search(main_data, 0)));
  writeln(IntToStr(exponential_search(main_data, 15)));
  writeln(IntToStr(exponential_search(main_data, 5)));
  writeln(IntToStr(exponential_search(main_data, 6)));
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
