{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
type IntArray = array of integer;
type NumberContainer = record
  numbermap: specialize TFPGMap<integer, IntArray>;
  indexmap: specialize TFPGMap<integer, integer>;
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
  nm: specialize TFPGMap<integer, IntArray>;
  im: specialize TFPGMap<integer, integer>;
  cont: NumberContainer;
  val: integer;
  index: integer;
  item: integer;
  array_: IntArray;
  num: integer;
  idx: integer;
  xs: IntArray;
function makeNumberContainer(numbermap: specialize TFPGMap<integer, IntArray>; indexmap: specialize TFPGMap<integer, integer>): NumberContainer; forward;
function remove_at(xs: IntArray; idx: integer): IntArray; forward;
function insert_at(xs: IntArray; idx: integer; val: integer): IntArray; forward;
function binary_search_delete(array_: IntArray; item: integer): IntArray; forward;
function binary_search_insert(array_: IntArray; index: integer): IntArray; forward;
function change(cont: NumberContainer; idx: integer; num: integer): NumberContainer; forward;
function find(cont: NumberContainer; num: integer): integer; forward;
function makeNumberContainer(numbermap: specialize TFPGMap<integer, IntArray>; indexmap: specialize TFPGMap<integer, integer>): NumberContainer;
begin
  Result.numbermap := numbermap;
  Result.indexmap := indexmap;
end;
function remove_at(xs: IntArray; idx: integer): IntArray;
var
  remove_at_res: array of integer;
  remove_at_i: integer;
begin
  remove_at_res := [];
  remove_at_i := 0;
  while remove_at_i < Length(xs) do begin
  if remove_at_i <> idx then begin
  remove_at_res := concat(remove_at_res, IntArray([xs[remove_at_i]]));
end;
  remove_at_i := remove_at_i + 1;
end;
  exit(remove_at_res);
end;
function insert_at(xs: IntArray; idx: integer; val: integer): IntArray;
var
  insert_at_res: array of integer;
  insert_at_i: integer;
begin
  insert_at_res := [];
  insert_at_i := 0;
  while insert_at_i < Length(xs) do begin
  if insert_at_i = idx then begin
  insert_at_res := concat(insert_at_res, IntArray([val]));
end;
  insert_at_res := concat(insert_at_res, IntArray([xs[insert_at_i]]));
  insert_at_i := insert_at_i + 1;
end;
  if idx = Length(xs) then begin
  insert_at_res := concat(insert_at_res, IntArray([val]));
end;
  exit(insert_at_res);
end;
function binary_search_delete(array_: IntArray; item: integer): IntArray;
var
  binary_search_delete_low: integer;
  binary_search_delete_high: integer;
  binary_search_delete_arr: array of integer;
  binary_search_delete_mid: integer;
begin
  binary_search_delete_low := 0;
  binary_search_delete_high := Length(array_) - 1;
  binary_search_delete_arr := array_;
  while binary_search_delete_low <= binary_search_delete_high do begin
  binary_search_delete_mid := (binary_search_delete_low + binary_search_delete_high) div 2;
  if binary_search_delete_arr[binary_search_delete_mid] = item then begin
  binary_search_delete_arr := remove_at(binary_search_delete_arr, binary_search_delete_mid);
  exit(binary_search_delete_arr);
end else begin
  if binary_search_delete_arr[binary_search_delete_mid] < item then begin
  binary_search_delete_low := binary_search_delete_mid + 1;
end else begin
  binary_search_delete_high := binary_search_delete_mid - 1;
end;
end;
end;
  writeln('ValueError: Either the item is not in the array or the array was unsorted');
  exit(binary_search_delete_arr);
end;
function binary_search_insert(array_: IntArray; index: integer): IntArray;
var
  binary_search_insert_low: integer;
  binary_search_insert_high: integer;
  binary_search_insert_arr: array of integer;
  binary_search_insert_mid: integer;
begin
  binary_search_insert_low := 0;
  binary_search_insert_high := Length(array_) - 1;
  binary_search_insert_arr := array_;
  while binary_search_insert_low <= binary_search_insert_high do begin
  binary_search_insert_mid := (binary_search_insert_low + binary_search_insert_high) div 2;
  if binary_search_insert_arr[binary_search_insert_mid] = index then begin
  binary_search_insert_arr := insert_at(binary_search_insert_arr, binary_search_insert_mid + 1, index);
  exit(binary_search_insert_arr);
end else begin
  if binary_search_insert_arr[binary_search_insert_mid] < index then begin
  binary_search_insert_low := binary_search_insert_mid + 1;
end else begin
  binary_search_insert_high := binary_search_insert_mid - 1;
end;
end;
end;
  binary_search_insert_arr := insert_at(binary_search_insert_arr, binary_search_insert_low, index);
  exit(binary_search_insert_arr);
end;
function change(cont: NumberContainer; idx: integer; num: integer): NumberContainer;
var
  change_numbermap: specialize TFPGMap<integer, IntArray>;
  change_indexmap: specialize TFPGMap<integer, integer>;
  change_old: integer;
  change_old_idx: integer;
  change_indexes: IntArray;
  change_indexes_idx: integer;
begin
  change_numbermap := cont.numbermap;
  change_indexmap := cont.indexmap;
  if change_indexmap.IndexOf(idx) <> -1 then begin
  change_old_idx := change_indexmap.IndexOf(idx);
  if change_old_idx <> -1 then begin
  change_old := change_indexmap.Data[change_old_idx];
end else begin
  change_old := 0;
end;
  change_indexes_idx := change_numbermap.IndexOf(change_old);
  if change_indexes_idx <> -1 then begin
  change_indexes := change_numbermap.Data[change_indexes_idx];
end else begin
  change_indexes := [];
end;
  if Length(change_indexes) = 1 then begin
  change_numbermap[change_old] := [];
end else begin
  change_numbermap[change_old] := binary_search_delete(change_indexes, idx);
end;
end;
  change_indexmap[idx] := num;
  if change_numbermap.IndexOf(num) <> -1 then begin
  change_numbermap[num] := binary_search_insert(change_numbermap[num], idx);
end else begin
  change_numbermap[num] := [idx];
end;
  exit(makeNumberContainer(change_numbermap, change_indexmap));
end;
function find(cont: NumberContainer; num: integer): integer;
var
  find_numbermap: specialize TFPGMap<integer, IntArray>;
  find_arr: IntArray;
  find_arr_idx: integer;
begin
  find_numbermap := cont.numbermap;
  if find_numbermap.IndexOf(num) <> -1 then begin
  find_arr_idx := find_numbermap.IndexOf(num);
  if find_arr_idx <> -1 then begin
  find_arr := find_numbermap.Data[find_arr_idx];
end else begin
  find_arr := [];
end;
  if Length(find_arr) > 0 then begin
  exit(find_arr[0]);
end;
end;
  exit(-1);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  nm := specialize TFPGMap<integer, IntArray>.Create();
  im := specialize TFPGMap<integer, integer>.Create();
  cont := makeNumberContainer(nm, im);
  writeln(find(cont, 10));
  cont := change(cont, 0, 10);
  writeln(find(cont, 10));
  cont := change(cont, 0, 20);
  writeln(find(cont, 10));
  writeln(find(cont, 20));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
