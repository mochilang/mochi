{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
type Node = record
  key: integer;
  value: integer;
  prev: integer;
  next: integer;
end;
type DoubleLinkedList = record
  nodes: array of Node;
  head: integer;
  tail: integer;
end;
type NodeArray = array of Node;
type LRUCache = record
  list: DoubleLinkedList;
  capacity: integer;
  num_keys: integer;
  hits: integer;
  misses: integer;
  cache: specialize TFPGMap<string, integer>;
end;
type GetResult = record
  cache: LRUCache;
  value: integer;
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
  cache: LRUCache;
  res: GetResult;
  key: integer;
  lst: DoubleLinkedList;
  cap: integer;
  c: LRUCache;
  value: integer;
  idx: integer;
function makeGetResult(cache: LRUCache; value: integer; ok: boolean): GetResult; forward;
function makeLRUCache(list: DoubleLinkedList; capacity: integer; num_keys: integer; hits: integer; misses: integer; cache: specialize TFPGMap<string, integer>): LRUCache; forward;
function makeDoubleLinkedList(nodes: NodeArray; head: integer; tail: integer): DoubleLinkedList; forward;
function makeNode(key: integer; value: integer; prev: integer; next: integer): Node; forward;
function new_list(): DoubleLinkedList; forward;
function dll_add(lst: DoubleLinkedList; idx: integer): DoubleLinkedList; forward;
function dll_remove(lst: DoubleLinkedList; idx: integer): DoubleLinkedList; forward;
function new_cache(cap: integer): LRUCache; forward;
function lru_get(c: LRUCache; key: integer): GetResult; forward;
function lru_put(c: LRUCache; key: integer; value: integer): LRUCache; forward;
function cache_info(cache: LRUCache): string; forward;
procedure print_result(res: GetResult); forward;
procedure main(); forward;
function makeGetResult(cache: LRUCache; value: integer; ok: boolean): GetResult;
begin
  Result.cache := cache;
  Result.value := value;
  Result.ok := ok;
end;
function makeLRUCache(list: DoubleLinkedList; capacity: integer; num_keys: integer; hits: integer; misses: integer; cache: specialize TFPGMap<string, integer>): LRUCache;
begin
  Result.list := list;
  Result.capacity := capacity;
  Result.num_keys := num_keys;
  Result.hits := hits;
  Result.misses := misses;
  Result.cache := cache;
end;
function makeDoubleLinkedList(nodes: NodeArray; head: integer; tail: integer): DoubleLinkedList;
begin
  Result.nodes := nodes;
  Result.head := head;
  Result.tail := tail;
end;
function makeNode(key: integer; value: integer; prev: integer; next: integer): Node;
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
function dll_add(lst: DoubleLinkedList; idx: integer): DoubleLinkedList;
var
  dll_add_nodes: array of Node;
  dll_add_tail_idx: integer;
  dll_add_tail_node: Node;
  dll_add_prev_idx: integer;
  dll_add_node_var: Node;
  dll_add_prev_node: Node;
begin
  dll_add_nodes := lst.nodes;
  dll_add_tail_idx := lst.tail;
  dll_add_tail_node := dll_add_nodes[dll_add_tail_idx];
  dll_add_prev_idx := dll_add_tail_node.prev;
  dll_add_node_var := dll_add_nodes[idx];
  dll_add_node_var.prev := dll_add_prev_idx;
  dll_add_node_var.next := dll_add_tail_idx;
  dll_add_nodes[idx] := dll_add_node_var;
  dll_add_prev_node := dll_add_nodes[dll_add_prev_idx];
  dll_add_prev_node.next := idx;
  dll_add_nodes[dll_add_prev_idx] := dll_add_prev_node;
  dll_add_tail_node.prev := idx;
  dll_add_nodes[dll_add_tail_idx] := dll_add_tail_node;
  lst.nodes := dll_add_nodes;
  exit(lst);
end;
function dll_remove(lst: DoubleLinkedList; idx: integer): DoubleLinkedList;
var
  dll_remove_nodes: array of Node;
  dll_remove_node_var: Node;
  dll_remove_prev_idx: integer;
  dll_remove_next_idx: integer;
  dll_remove_prev_node: Node;
  dll_remove_next_node: Node;
begin
  dll_remove_nodes := lst.nodes;
  dll_remove_node_var := dll_remove_nodes[idx];
  dll_remove_prev_idx := dll_remove_node_var.prev;
  dll_remove_next_idx := dll_remove_node_var.next;
  if (dll_remove_prev_idx = (0 - 1)) or (dll_remove_next_idx = (0 - 1)) then begin
  exit(lst);
end;
  dll_remove_prev_node := dll_remove_nodes[dll_remove_prev_idx];
  dll_remove_prev_node.next := dll_remove_next_idx;
  dll_remove_nodes[dll_remove_prev_idx] := dll_remove_prev_node;
  dll_remove_next_node := dll_remove_nodes[dll_remove_next_idx];
  dll_remove_next_node.prev := dll_remove_prev_idx;
  dll_remove_nodes[dll_remove_next_idx] := dll_remove_next_node;
  dll_remove_node_var.prev := 0 - 1;
  dll_remove_node_var.next := 0 - 1;
  dll_remove_nodes[idx] := dll_remove_node_var;
  lst.nodes := dll_remove_nodes;
  exit(lst);
end;
function new_cache(cap: integer): LRUCache;
var
  new_cache_empty_map: specialize TFPGMap<string, integer>;
begin
  new_cache_empty_map := specialize TFPGMap<string, integer>.Create();
  exit(makeLRUCache(new_list(), cap, 0, 0, 0, new_cache_empty_map));
end;
function lru_get(c: LRUCache; key: integer): GetResult;
var
  lru_get_cache: LRUCache;
  lru_get_key_str: string;
  lru_get_idx: integer;
  lru_get_idx_idx: integer;
  lru_get_node_var: Node;
  lru_get_value: integer;
begin
  lru_get_cache := c;
  lru_get_key_str := IntToStr(key);
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
function lru_put(c: LRUCache; key: integer; value: integer): LRUCache;
var
  lru_put_cache: LRUCache;
  lru_put_key_str: string;
  lru_put_head_node: Node;
  lru_put_first_idx: integer;
  lru_put_first_node: Node;
  lru_put_old_key: integer;
  lru_put_mdel: specialize TFPGMap<string, integer>;
  lru_put_nodes: array of Node;
  lru_put_new_node: Node;
  lru_put_idx: integer;
  lru_put_m: specialize TFPGMap<string, integer>;
  lru_put_idx_idx: integer;
  lru_put_node_var: Node;
begin
  lru_put_cache := c;
  lru_put_key_str := IntToStr(key);
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
  lru_put_new_node := makeNode(key, value, 0 - 1, 0 - 1);
  lru_put_nodes := concat(lru_put_nodes, [lru_put_new_node]);
  lru_put_idx := Length(lru_put_nodes) - 1;
  lru_put_cache.list.nodes := lru_put_nodes;
  lru_put_cache.list := dll_add(lru_put_cache.list, lru_put_idx);
  lru_put_m := lru_put_cache.cache;
  lru_put_m[lru_put_key_str] := lru_put_idx;
  lru_put_cache.cache := lru_put_m;
  lru_put_cache.num_keys := lru_put_cache.num_keys + 1;
end else begin
  lru_put_m := lru_put_cache.cache;
  lru_put_idx_idx := lru_put_m.IndexOf(lru_put_key_str);
  if lru_put_idx_idx <> -1 then begin
  lru_put_idx := lru_put_m.Data[lru_put_idx_idx];
end else begin
  lru_put_idx := 0;
end;
  lru_put_nodes := lru_put_cache.list.nodes;
  lru_put_node_var := lru_put_nodes[lru_put_idx];
  lru_put_node_var.value := value;
  lru_put_nodes[lru_put_idx] := lru_put_node_var;
  lru_put_cache.list.nodes := lru_put_nodes;
  lru_put_cache.list := dll_remove(lru_put_cache.list, lru_put_idx);
  lru_put_cache.list := dll_add(lru_put_cache.list, lru_put_idx);
  lru_put_cache.cache := lru_put_m;
end;
  exit(lru_put_cache);
end;
function cache_info(cache: LRUCache): string;
begin
  exit(((((((('CacheInfo(hits=' + IntToStr(cache.hits)) + ', misses=') + IntToStr(cache.misses)) + ', capacity=') + IntToStr(cache.capacity)) + ', current size=') + IntToStr(cache.num_keys)) + ')');
end;
procedure print_result(res: GetResult);
begin
  if res.ok then begin
  writeln(IntToStr(res.value));
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
end.
