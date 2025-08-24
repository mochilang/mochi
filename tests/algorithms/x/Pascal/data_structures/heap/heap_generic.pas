{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
type FuncType1 = function(arg0: int64): int64 is nested;
type IntArray = array of int64;
type IntArrayArray = array of IntArray;
type Heap = record
  arr: array of IntArray;
  pos_map: specialize TFPGMap<int64, int64>;
  size: int64;
  key: FuncType1;
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
procedure json(x: int64);
begin
  writeln(x);
end;
function list_int_to_str(xs: array of int64): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  h: Heap;
function Map1(): specialize TFPGMap<int64, int64>; forward;
function makeHeap(arr: IntArrayArray; pos_map: specialize TFPGMap<int64, int64>; size: int64; key: FuncType1): Heap; forward;
function new_heap(new_heap_key: FuncType1): Heap; forward;
function parent(parent_i: int64): int64; forward;
function left(left_i: int64; left_size: int64): int64; forward;
function right(right_i: int64; right_size: int64): int64; forward;
procedure swap(swap_h: Heap; swap_i: int64; swap_j: int64); forward;
function cmp(cmp_h: Heap; cmp_i: int64; cmp_j: int64): boolean; forward;
function get_valid_parent(get_valid_parent_h: Heap; get_valid_parent_i: int64): int64; forward;
procedure heapify_up(heapify_up_h: Heap; heapify_up_index: int64); forward;
procedure heapify_down(heapify_down_h: Heap; heapify_down_index: int64); forward;
procedure update_item(update_item_h: Heap; update_item_item: int64; update_item_item_value: int64); forward;
procedure delete_item(delete_item_h: Heap; delete_item_item: int64); forward;
procedure insert_item(insert_item_h: Heap; insert_item_item: int64; insert_item_item_value: int64); forward;
function get_top(get_top_h: Heap): IntArray; forward;
function extract_top(extract_top_h: Heap): IntArray; forward;
function identity(identity_x: int64): int64; forward;
function negate(negate_x: int64): int64; forward;
function Map1(): specialize TFPGMap<int64, int64>;
begin
  Result := specialize TFPGMap<int64, int64>.Create();
end;
function makeHeap(arr: IntArrayArray; pos_map: specialize TFPGMap<int64, int64>; size: int64; key: FuncType1): Heap;
begin
  Result.arr := arr;
  Result.pos_map := pos_map;
  Result.size := size;
  Result.key := key;
end;
function new_heap(new_heap_key: FuncType1): Heap;
begin
  exit(makeHeap([], Map1(), 0, new_heap_key));
end;
function parent(parent_i: int64): int64;
begin
  if parent_i > 0 then begin
  exit((parent_i - 1) div 2);
end;
  exit(-1);
end;
function left(left_i: int64; left_size: int64): int64;
var
  left_l: int64;
begin
  left_l := (2 * left_i) + 1;
  if left_l < left_size then begin
  exit(left_l);
end;
  exit(-1);
end;
function right(right_i: int64; right_size: int64): int64;
var
  right_r: int64;
begin
  right_r := (2 * right_i) + 2;
  if right_r < right_size then begin
  exit(right_r);
end;
  exit(-1);
end;
procedure swap(swap_h: Heap; swap_i: int64; swap_j: int64);
var
  swap_arr: array of IntArray;
  swap_item_i: int64;
  swap_item_j: int64;
  swap_pm: specialize TFPGMap<int64, int64>;
  swap_tmp: array of int64;
begin
  swap_arr := swap_h.arr;
  swap_item_i := swap_arr[swap_i][0];
  swap_item_j := swap_arr[swap_j][0];
  swap_pm := swap_h.pos_map;
  swap_pm[swap_item_i] := swap_j + 1;
  swap_pm[swap_item_j] := swap_i + 1;
  swap_h.pos_map := swap_pm;
  swap_tmp := swap_arr[swap_i];
  swap_arr[swap_i] := swap_arr[swap_j];
  swap_arr[swap_j] := swap_tmp;
  swap_h.arr := swap_arr;
end;
function cmp(cmp_h: Heap; cmp_i: int64; cmp_j: int64): boolean;
var
  cmp_arr: array of IntArray;
begin
  cmp_arr := cmp_h.arr;
  exit(cmp_arr[cmp_i][1] < cmp_arr[cmp_j][1]);
end;
function get_valid_parent(get_valid_parent_h: Heap; get_valid_parent_i: int64): int64;
var
  get_valid_parent_vp: int64;
  get_valid_parent_l: int64;
  get_valid_parent_r: int64;
begin
  get_valid_parent_vp := get_valid_parent_i;
  get_valid_parent_l := left(get_valid_parent_i, get_valid_parent_h.size);
  if (get_valid_parent_l <> (0 - 1)) and (cmp(get_valid_parent_h, get_valid_parent_l, get_valid_parent_vp) = false) then begin
  get_valid_parent_vp := get_valid_parent_l;
end;
  get_valid_parent_r := right(get_valid_parent_i, get_valid_parent_h.size);
  if (get_valid_parent_r <> (0 - 1)) and (cmp(get_valid_parent_h, get_valid_parent_r, get_valid_parent_vp) = false) then begin
  get_valid_parent_vp := get_valid_parent_r;
end;
  exit(get_valid_parent_vp);
end;
procedure heapify_up(heapify_up_h: Heap; heapify_up_index: int64);
var
  heapify_up_idx: int64;
  heapify_up_p: int64;
begin
  heapify_up_idx := heapify_up_index;
  heapify_up_p := parent(heapify_up_idx);
  while (heapify_up_p <> (0 - 1)) and (cmp(heapify_up_h, heapify_up_idx, heapify_up_p) = false) do begin
  swap(heapify_up_h, heapify_up_idx, heapify_up_p);
  heapify_up_idx := heapify_up_p;
  heapify_up_p := parent(heapify_up_p);
end;
end;
procedure heapify_down(heapify_down_h: Heap; heapify_down_index: int64);
var
  heapify_down_idx: int64;
  heapify_down_vp: int64;
begin
  heapify_down_idx := heapify_down_index;
  heapify_down_vp := get_valid_parent(heapify_down_h, heapify_down_idx);
  while heapify_down_vp <> heapify_down_idx do begin
  swap(heapify_down_h, heapify_down_idx, heapify_down_vp);
  heapify_down_idx := heapify_down_vp;
  heapify_down_vp := get_valid_parent(heapify_down_h, heapify_down_idx);
end;
end;
procedure update_item(update_item_h: Heap; update_item_item: int64; update_item_item_value: int64);
var
  update_item_pm: specialize TFPGMap<int64, int64>;
  update_item_index: int64;
  update_item_arr: array of IntArray;
begin
  update_item_pm := update_item_h.pos_map;
  if update_item_pm[update_item_item] = 0 then begin
  exit();
end;
  update_item_index := update_item_pm[update_item_item] - 1;
  update_item_arr := update_item_h.arr;
  update_item_arr[update_item_index] := [update_item_item, update_item_h.key(update_item_item_value)];
  update_item_h.arr := update_item_arr;
  update_item_h.pos_map := update_item_pm;
  heapify_up(update_item_h, update_item_index);
  heapify_down(update_item_h, update_item_index);
end;
procedure delete_item(delete_item_h: Heap; delete_item_item: int64);
var
  delete_item_pm: specialize TFPGMap<int64, int64>;
  delete_item_index: int64;
  delete_item_arr: array of IntArray;
  delete_item_last_index: int64;
  delete_item_moved: int64;
begin
  delete_item_pm := delete_item_h.pos_map;
  if delete_item_pm[delete_item_item] = 0 then begin
  exit();
end;
  delete_item_index := delete_item_pm[delete_item_item] - 1;
  delete_item_pm[delete_item_item] := 0;
  delete_item_arr := delete_item_h.arr;
  delete_item_last_index := delete_item_h.size - 1;
  if delete_item_index <> delete_item_last_index then begin
  delete_item_arr[delete_item_index] := delete_item_arr[delete_item_last_index];
  delete_item_moved := delete_item_arr[delete_item_index][0];
  delete_item_pm[delete_item_moved] := delete_item_index + 1;
end;
  delete_item_h.size := delete_item_h.size - 1;
  delete_item_h.arr := delete_item_arr;
  delete_item_h.pos_map := delete_item_pm;
  if delete_item_h.size > delete_item_index then begin
  heapify_up(delete_item_h, delete_item_index);
  heapify_down(delete_item_h, delete_item_index);
end;
end;
procedure insert_item(insert_item_h: Heap; insert_item_item: int64; insert_item_item_value: int64);
var
  insert_item_arr: array of IntArray;
  insert_item_arr_len: int64;
  insert_item_pm: specialize TFPGMap<int64, int64>;
begin
  insert_item_arr := insert_item_h.arr;
  insert_item_arr_len := Length(insert_item_arr);
  if insert_item_arr_len = insert_item_h.size then begin
  insert_item_arr := concat(insert_item_arr, [[insert_item_item, insert_item_h.key(insert_item_item_value)]]);
end else begin
  insert_item_arr[insert_item_h.size] := [insert_item_item, insert_item_h.key(insert_item_item_value)];
end;
  insert_item_pm := insert_item_h.pos_map;
  insert_item_pm[insert_item_item] := insert_item_h.size + 1;
  insert_item_h.size := insert_item_h.size + 1;
  insert_item_h.arr := insert_item_arr;
  insert_item_h.pos_map := insert_item_pm;
  heapify_up(insert_item_h, insert_item_h.size - 1);
end;
function get_top(get_top_h: Heap): IntArray;
var
  get_top_arr: array of IntArray;
begin
  get_top_arr := get_top_h.arr;
  if get_top_h.size > 0 then begin
  exit(get_top_arr[0]);
end;
  exit([]);
end;
function extract_top(extract_top_h: Heap): IntArray;
var
  extract_top_top: IntArray;
begin
  extract_top_top := get_top(extract_top_h);
  if Length(extract_top_top) > 0 then begin
  delete_item(extract_top_h, extract_top_top[0]);
end;
  exit(extract_top_top);
end;
function identity(identity_x: int64): int64;
begin
  exit(identity_x);
end;
function negate(negate_x: int64): int64;
begin
  exit(0 - negate_x);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  h := new_heap(@identity);
  insert_item(h, 5, 34);
  insert_item(h, 6, 31);
  insert_item(h, 7, 37);
  writeln(list_int_to_str(get_top(h)));
  writeln(list_int_to_str(extract_top(h)));
  writeln(list_int_to_str(extract_top(h)));
  writeln(list_int_to_str(extract_top(h)));
  h := new_heap(@negate);
  insert_item(h, 5, 34);
  insert_item(h, 6, 31);
  insert_item(h, 7, 37);
  writeln(list_int_to_str(get_top(h)));
  writeln(list_int_to_str(extract_top(h)));
  writeln(list_int_to_str(extract_top(h)));
  writeln(list_int_to_str(extract_top(h)));
  insert_item(h, 8, 45);
  insert_item(h, 9, 40);
  insert_item(h, 10, 50);
  writeln(list_int_to_str(get_top(h)));
  update_item(h, 10, 30);
  writeln(list_int_to_str(get_top(h)));
  delete_item(h, 10);
  writeln(list_int_to_str(get_top(h)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
