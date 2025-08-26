{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
type Node = record
  key: int64;
  value: int64;
  prev: int64;
  next: int64;
end;
type NodeArray = array of Node;
type DoubleLinkedList = record
  nodes: array of Node;
  head: int64;
  tail: int64;
end;
type LRUCache = record
  list: DoubleLinkedList;
  capacity: int64;
  num_keys: int64;
  hits: int64;
  misses: int64;
  cache: specialize TFPGMap<string, int64>;
end;
type GetResult = record
  cache: LRUCache;
  value: int64;
  ok: boolean;
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
function makeGetResult(cache: LRUCache; value: int64; ok: boolean): GetResult; forward;
function makeLRUCache(list: DoubleLinkedList; capacity: int64; num_keys: int64; hits: int64; misses: int64; cache: specialize TFPGMap<string, int64>): LRUCache; forward;
function makeDoubleLinkedList(nodes: NodeArray; head: int64; tail: int64): DoubleLinkedList; forward;
function makeNode(key: int64; value: int64; prev: int64; next: int64): Node; forward;
function new_list(): DoubleLinkedList; forward;
function dll_add(dll_add_lst: DoubleLinkedList; dll_add_idx: int64): DoubleLinkedList; forward;
function dll_remove(dll_remove_lst: DoubleLinkedList; dll_remove_idx: int64): DoubleLinkedList; forward;
function new_cache(new_cache_cap: int64): LRUCache; forward;
function lru_get(lru_get_c: LRUCache; lru_get_key: int64): GetResult; forward;
function lru_put(lru_put_c: LRUCache; lru_put_key: int64; lru_put_value: int64): LRUCache; forward;
function cache_info(cache_info_cache: LRUCache): string; forward;
procedure print_result(print_result_res: GetResult); forward;
procedure main(); forward;
function makeGetResult(cache: LRUCache; value: int64; ok: boolean): GetResult;
begin
  Result.cache := cache;
  Result.value := value;
  Result.ok := ok;
end;
function makeLRUCache(list: DoubleLinkedList; capacity: int64; num_keys: int64; hits: int64; misses: int64; cache: specialize TFPGMap<string, int64>): LRUCache;
begin
  Result.list := list;
  Result.capacity := capacity;
  Result.num_keys := num_keys;
  Result.hits := hits;
  Result.misses := misses;
  Result.cache := cache;
end;
function makeDoubleLinkedList(nodes: NodeArray; head: int64; tail: int64): DoubleLinkedList;
begin
  Result.nodes := nodes;
  Result.head := head;
  Result.tail := tail;
end;
function makeNode(key: int64; value: int64; prev: int64; next: int64): Node;
begin
  Result.key := key;
  Result.value := value;
  Result.prev := prev;
  Result.next := next;
end;
function new_list(): DoubleLinkedList;
var
  new_list_nodes: array of Node;
  new_list_head: Node;
  new_list_tail: Node;
begin
  new_list_nodes := [];
  new_list_head := makeNode(0, 0, 0 - 1, 1);
  new_list_tail := makeNode(0, 0, 0, 0 - 1);
  new_list_nodes := concat(new_list_nodes, [new_list_head]);
  new_list_nodes := concat(new_list_nodes, [new_list_tail]);
  exit(makeDoubleLinkedList(new_list_nodes, 0, 1));
end;
function dll_add(dll_add_lst: DoubleLinkedList; dll_add_idx: int64): DoubleLinkedList;
var
  dll_add_nodes: array of Node;
  dll_add_tail_idx: int64;
  dll_add_tail_node: Node;
  dll_add_prev_idx: int64;
  dll_add_node_var: Node;
  dll_add_prev_node: Node;
begin
  dll_add_nodes := dll_add_lst.nodes;
  dll_add_tail_idx := dll_add_lst.tail;
  dll_add_tail_node := dll_add_nodes[dll_add_tail_idx];
  dll_add_prev_idx := dll_add_tail_node.prev;
  dll_add_node_var := dll_add_nodes[dll_add_idx];
  dll_add_node_var.prev := dll_add_prev_idx;
  dll_add_node_var.next := dll_add_tail_idx;
  dll_add_nodes[dll_add_idx] := dll_add_node_var;
  dll_add_prev_node := dll_add_nodes[dll_add_prev_idx];
  dll_add_prev_node.next := dll_add_idx;
  dll_add_nodes[dll_add_prev_idx] := dll_add_prev_node;
  dll_add_tail_node.prev := dll_add_idx;
  dll_add_nodes[dll_add_tail_idx] := dll_add_tail_node;
  dll_add_lst.nodes := dll_add_nodes;
  exit(dll_add_lst);
end;
function dll_remove(dll_remove_lst: DoubleLinkedList; dll_remove_idx: int64): DoubleLinkedList;
var
  dll_remove_nodes: array of Node;
  dll_remove_node_var: Node;
  dll_remove_prev_idx: int64;
  dll_remove_next_idx: int64;
  dll_remove_prev_node: Node;
  dll_remove_next_node: Node;
begin
  dll_remove_nodes := dll_remove_lst.nodes;
  dll_remove_node_var := dll_remove_nodes[dll_remove_idx];
  dll_remove_prev_idx := dll_remove_node_var.prev;
  dll_remove_next_idx := dll_remove_node_var.next;
  if (dll_remove_prev_idx = (0 - 1)) or (dll_remove_next_idx = (0 - 1)) then begin
  exit(dll_remove_lst);
end;
  dll_remove_prev_node := dll_remove_nodes[dll_remove_prev_idx];
  dll_remove_prev_node.next := dll_remove_next_idx;
  dll_remove_nodes[dll_remove_prev_idx] := dll_remove_prev_node;
  dll_remove_next_node := dll_remove_nodes[dll_remove_next_idx];
  dll_remove_next_node.prev := dll_remove_prev_idx;
  dll_remove_nodes[dll_remove_next_idx] := dll_remove_next_node;
  dll_remove_node_var.prev := 0 - 1;
  dll_remove_node_var.next := 0 - 1;
  dll_remove_nodes[dll_remove_idx] := dll_remove_node_var;
  dll_remove_lst.nodes := dll_remove_nodes;
  exit(dll_remove_lst);
end;
function new_cache(new_cache_cap: int64): LRUCache;
var
  new_cache_empty_map: specialize TFPGMap<string, int64>;
begin
  new_cache_empty_map := specialize TFPGMap<string, int64>.Create();
  exit(makeLRUCache(new_list(), new_cache_cap, 0, 0, 0, new_cache_empty_map));
end;
function lru_get(lru_get_c: LRUCache; lru_get_key: int64): GetResult;
var
  lru_get_cache: LRUCache;
  lru_get_key_str: string;
  lru_get_idx: int64;
  lru_get_idx_idx: integer;
  lru_get_node_var: Node;
  lru_get_value: int64;
begin
  lru_get_cache := lru_get_c;
  lru_get_key_str := IntToStr(lru_get_key);
  if lru_get_cache.cache.IndexOf(lru_get_key_str) <> -1 then begin
  lru_get_idx_idx := lru_get_cache.cache.IndexOf(lru_get_key_str);
  if lru_get_idx_idx <> -1 then begin
  lru_get_idx := lru_get_cache.cache.Data[lru_get_idx_idx];
end else begin
  lru_get_idx := 0;
end;
  if lru_get_idx <> (0 - 1) then begin
  lru_get_cache.hits := lru_get_cache.hits + 1;
  lru_get_node_var := lru_get_cache.list.nodes[lru_get_idx];
  lru_get_value := lru_get_node_var.value;
  lru_get_cache.list := dll_remove(lru_get_cache.list, lru_get_idx);
  lru_get_cache.list := dll_add(lru_get_cache.list, lru_get_idx);
  exit(makeGetResult(lru_get_cache, lru_get_value, true));
end;
end;
  lru_get_cache.misses := lru_get_cache.misses + 1;
  exit(makeGetResult(lru_get_cache, 0, false));
end;
function lru_put(lru_put_c: LRUCache; lru_put_key: int64; lru_put_value: int64): LRUCache;
var
  lru_put_cache: LRUCache;
  lru_put_key_str: string;
  lru_put_head_node: Node;
  lru_put_first_idx: int64;
  lru_put_first_node: Node;
  lru_put_old_key: int64;
  lru_put_mdel: specialize TFPGMap<string, int64>;
  lru_put_nodes: array of Node;
  lru_put_new_node: Node;
  lru_put_idx: integer;
  lru_put_m: specialize TFPGMap<string, int64>;
  lru_put_m_37: specialize TFPGMap<string, int64>;
  lru_put_idx_38: int64;
  lru_put_idx_38_idx: integer;
  lru_put_nodes_40: array of Node;
  lru_put_node_var: Node;
begin
  lru_put_cache := lru_put_c;
  lru_put_key_str := IntToStr(lru_put_key);
  if not lru_put_cache.cache.IndexOf(lru_put_key_str) <> -1 then begin
  if lru_put_cache.num_keys >= lru_put_cache.capacity then begin
  lru_put_head_node := lru_put_cache.list.nodes[lru_put_cache.list.head];
  lru_put_first_idx := lru_put_head_node.next;
  lru_put_first_node := lru_put_cache.list.nodes[lru_put_first_idx];
  lru_put_old_key := lru_put_first_node.key;
  lru_put_cache.list := dll_remove(lru_put_cache.list, lru_put_first_idx);
  lru_put_mdel := lru_put_cache.cache;
  lru_put_mdel[IntToStr(lru_put_old_key)] := 0 - 1;
  lru_put_cache.cache := lru_put_mdel;
  lru_put_cache.num_keys := lru_put_cache.num_keys - 1;
end;
  lru_put_nodes := lru_put_cache.list.nodes;
  lru_put_new_node := makeNode(lru_put_key, lru_put_value, 0 - 1, 0 - 1);
  lru_put_nodes := concat(lru_put_nodes, [lru_put_new_node]);
  lru_put_idx := Length(lru_put_nodes) - 1;
  lru_put_cache.list.nodes := lru_put_nodes;
  lru_put_cache.list := dll_add(lru_put_cache.list, lru_put_idx);
  lru_put_m := lru_put_cache.cache;
  lru_put_m[lru_put_key_str] := lru_put_idx;
  lru_put_cache.cache := lru_put_m;
  lru_put_cache.num_keys := lru_put_cache.num_keys + 1;
end else begin
  lru_put_m_37 := lru_put_cache.cache;
  lru_put_idx_38_idx := lru_put_m_37.IndexOf(lru_put_key_str);
  if lru_put_idx_38_idx <> -1 then begin
  lru_put_idx_38 := lru_put_m_37.Data[lru_put_idx_38_idx];
end else begin
  lru_put_idx_38 := 0;
end;
  lru_put_nodes_40 := lru_put_cache.list.nodes;
  lru_put_node_var := lru_put_nodes_40[lru_put_idx_38];
  lru_put_node_var.value := lru_put_value;
  lru_put_nodes_40[lru_put_idx_38] := lru_put_node_var;
  lru_put_cache.list.nodes := lru_put_nodes_40;
  lru_put_cache.list := dll_remove(lru_put_cache.list, lru_put_idx_38);
  lru_put_cache.list := dll_add(lru_put_cache.list, lru_put_idx_38);
  lru_put_cache.cache := lru_put_m_37;
end;
  exit(lru_put_cache);
end;
function cache_info(cache_info_cache: LRUCache): string;
begin
  exit(((((((('CacheInfo(hits=' + IntToStr(cache_info_cache.hits)) + ', misses=') + IntToStr(cache_info_cache.misses)) + ', capacity=') + IntToStr(cache_info_cache.capacity)) + ', current size=') + IntToStr(cache_info_cache.num_keys)) + ')');
end;
procedure print_result(print_result_res: GetResult);
begin
  if print_result_res.ok then begin
  writeln(IntToStr(print_result_res.value));
end else begin
  writeln('None');
end;
end;
procedure main();
var
  main_cache: LRUCache;
  main_r1: GetResult;
  main_r2: GetResult;
  main_r3: GetResult;
  main_r4: GetResult;
  main_r5: GetResult;
begin
  main_cache := new_cache(2);
  main_cache := lru_put(main_cache, 1, 1);
  main_cache := lru_put(main_cache, 2, 2);
  main_r1 := lru_get(main_cache, 1);
  main_cache := main_r1.cache;
  print_result(main_r1);
  main_cache := lru_put(main_cache, 3, 3);
  main_r2 := lru_get(main_cache, 2);
  main_cache := main_r2.cache;
  print_result(main_r2);
  main_cache := lru_put(main_cache, 4, 4);
  main_r3 := lru_get(main_cache, 1);
  main_cache := main_r3.cache;
  print_result(main_r3);
  main_r4 := lru_get(main_cache, 3);
  main_cache := main_r4.cache;
  print_result(main_r4);
  main_r5 := lru_get(main_cache, 4);
  main_cache := main_r5.cache;
  print_result(main_r5);
  writeln(cache_info(main_cache));
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
