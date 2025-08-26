{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
type IntArray = array of int64;
type NumberContainer = record
  numbermap: specialize TFPGMap<int64, IntArray>;
  indexmap: specialize TFPGMap<int64, int64>;
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
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: int64): real;
begin
  _to_float := x;
end;
function to_float(x: int64): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  nm: specialize TFPGMap<int64, IntArray>;
  im: specialize TFPGMap<int64, int64>;
  cont: NumberContainer;
function makeNumberContainer(numbermap: specialize TFPGMap<int64, IntArray>; indexmap: specialize TFPGMap<int64, int64>): NumberContainer; forward;
function remove_at(remove_at_xs: IntArray; remove_at_idx: int64): IntArray; forward;
function insert_at(insert_at_xs: IntArray; insert_at_idx: int64; insert_at_val: int64): IntArray; forward;
function binary_search_delete(binary_search_delete_array_: IntArray; binary_search_delete_item: int64): IntArray; forward;
function binary_search_insert(binary_search_insert_array_: IntArray; binary_search_insert_index: int64): IntArray; forward;
function change(change_cont: NumberContainer; change_idx: int64; change_num: int64): NumberContainer; forward;
function find(find_cont: NumberContainer; find_num: int64): int64; forward;
function makeNumberContainer(numbermap: specialize TFPGMap<int64, IntArray>; indexmap: specialize TFPGMap<int64, int64>): NumberContainer;
begin
  Result.numbermap := numbermap;
  Result.indexmap := indexmap;
end;
function remove_at(remove_at_xs: IntArray; remove_at_idx: int64): IntArray;
var
  remove_at_res: array of int64;
  remove_at_i: int64;
begin
  remove_at_res := [];
  remove_at_i := 0;
  while remove_at_i < Length(remove_at_xs) do begin
  if remove_at_i <> remove_at_idx then begin
  remove_at_res := concat(remove_at_res, IntArray([remove_at_xs[remove_at_i]]));
end;
  remove_at_i := remove_at_i + 1;
end;
  exit(remove_at_res);
end;
function insert_at(insert_at_xs: IntArray; insert_at_idx: int64; insert_at_val: int64): IntArray;
var
  insert_at_res: array of int64;
  insert_at_i: int64;
begin
  insert_at_res := [];
  insert_at_i := 0;
  while insert_at_i < Length(insert_at_xs) do begin
  if insert_at_i = insert_at_idx then begin
  insert_at_res := concat(insert_at_res, IntArray([insert_at_val]));
end;
  insert_at_res := concat(insert_at_res, IntArray([insert_at_xs[insert_at_i]]));
  insert_at_i := insert_at_i + 1;
end;
  if insert_at_idx = Length(insert_at_xs) then begin
  insert_at_res := concat(insert_at_res, IntArray([insert_at_val]));
end;
  exit(insert_at_res);
end;
function binary_search_delete(binary_search_delete_array_: IntArray; binary_search_delete_item: int64): IntArray;
var
  binary_search_delete_low: int64;
  binary_search_delete_high: integer;
  binary_search_delete_arr: array of int64;
  binary_search_delete_mid: int64;
begin
  binary_search_delete_low := 0;
  binary_search_delete_high := Length(binary_search_delete_array_) - 1;
  binary_search_delete_arr := binary_search_delete_array_;
  while binary_search_delete_low <= binary_search_delete_high do begin
  binary_search_delete_mid := _floordiv(binary_search_delete_low + binary_search_delete_high, 2);
  if binary_search_delete_arr[binary_search_delete_mid] = binary_search_delete_item then begin
  binary_search_delete_arr := remove_at(binary_search_delete_arr, binary_search_delete_mid);
  exit(binary_search_delete_arr);
end else begin
  if binary_search_delete_arr[binary_search_delete_mid] < binary_search_delete_item then begin
  binary_search_delete_low := binary_search_delete_mid + 1;
end else begin
  binary_search_delete_high := binary_search_delete_mid - 1;
end;
end;
end;
  writeln('ValueError: Either the item is not in the array or the array was unsorted');
  exit(binary_search_delete_arr);
end;
function binary_search_insert(binary_search_insert_array_: IntArray; binary_search_insert_index: int64): IntArray;
var
  binary_search_insert_low: int64;
  binary_search_insert_high: integer;
  binary_search_insert_arr: array of int64;
  binary_search_insert_mid: int64;
begin
  binary_search_insert_low := 0;
  binary_search_insert_high := Length(binary_search_insert_array_) - 1;
  binary_search_insert_arr := binary_search_insert_array_;
  while binary_search_insert_low <= binary_search_insert_high do begin
  binary_search_insert_mid := _floordiv(binary_search_insert_low + binary_search_insert_high, 2);
  if binary_search_insert_arr[binary_search_insert_mid] = binary_search_insert_index then begin
  binary_search_insert_arr := insert_at(binary_search_insert_arr, binary_search_insert_mid + 1, binary_search_insert_index);
  exit(binary_search_insert_arr);
end else begin
  if binary_search_insert_arr[binary_search_insert_mid] < binary_search_insert_index then begin
  binary_search_insert_low := binary_search_insert_mid + 1;
end else begin
  binary_search_insert_high := binary_search_insert_mid - 1;
end;
end;
end;
  binary_search_insert_arr := insert_at(binary_search_insert_arr, binary_search_insert_low, binary_search_insert_index);
  exit(binary_search_insert_arr);
end;
function change(change_cont: NumberContainer; change_idx: int64; change_num: int64): NumberContainer;
var
  change_numbermap: specialize TFPGMap<int64, IntArray>;
  change_indexmap: specialize TFPGMap<int64, int64>;
  change_old: int64;
  change_old_idx: integer;
  change_indexes: IntArray;
  change_indexes_idx: integer;
begin
  change_numbermap := change_cont.numbermap;
  change_indexmap := change_cont.indexmap;
  if change_indexmap.IndexOf(change_idx) <> -1 then begin
  change_old_idx := change_indexmap.IndexOf(change_idx);
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
  change_numbermap[change_old] := binary_search_delete(change_indexes, change_idx);
end;
end;
  change_indexmap[change_idx] := change_num;
  if change_numbermap.IndexOf(change_num) <> -1 then begin
  change_numbermap[change_num] := binary_search_insert(change_numbermap[change_num], change_idx);
end else begin
  change_numbermap[change_num] := [change_idx];
end;
  exit(makeNumberContainer(change_numbermap, change_indexmap));
end;
function find(find_cont: NumberContainer; find_num: int64): int64;
var
  find_numbermap: specialize TFPGMap<int64, IntArray>;
  find_arr: IntArray;
  find_arr_idx: integer;
begin
  find_numbermap := find_cont.numbermap;
  if find_numbermap.IndexOf(find_num) <> -1 then begin
  find_arr_idx := find_numbermap.IndexOf(find_num);
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
  nm := specialize TFPGMap<int64, IntArray>.Create();
  im := specialize TFPGMap<int64, int64>.Create();
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
  writeln('');
end.
