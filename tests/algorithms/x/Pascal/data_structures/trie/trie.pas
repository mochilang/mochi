{$mode objfpc}
program Main;
uses SysUtils, Variants, fgl;
type Node = record
  children: specialize TFPGMap<string, integer>;
  is_leaf: boolean;
end;
type Trie = record
  nodes: array of Node;
end;
type NodeArray = array of Node;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  remove_key_key_idx: integer;
  _delete_node_var: Node;
  _delete_children: specialize TFPGMap<string, integer>;
  _delete_ch: string;
  _delete_child_idx: integer;
  _delete_child_idx_idx: integer;
  _delete_should_delete: Variant;
  _delete_new_children: specialize TFPGMap<string, integer>;
  dfs_node_var: Node;
  dfs_key: string;
  dfs_key_idx: integer;
  trie_var: Trie;
  word: string;
  passes: boolean;
  m: specialize TFPGMap<string, integer>;
  msg: string;
  k: string;
  words: StrArray;
function Map1(): specialize TFPGMap<string, integer>; forward;
function makeTrie(nodes: NodeArray): Trie; forward;
function makeNode(children: specialize TFPGMap<string, integer>; is_leaf: boolean): Node; forward;
function new_trie(): Trie; forward;
function remove_key(m: specialize TFPGMap<string, integer>; k: string): specialize TFPGMap<string, integer>; forward;
procedure insert(trie_var: Trie; word: string); forward;
procedure insert_many(trie_var: Trie; words: StrArray); forward;
function find(trie_var: Trie; word: string): boolean; forward;
procedure delete(trie_var: Trie; word: string); forward;
procedure print_words(trie_var: Trie); forward;
function test_trie(): boolean; forward;
procedure print_results(msg: string; passes: boolean); forward;
function Map1(): specialize TFPGMap<string, integer>;
begin
  Result := specialize TFPGMap<string, integer>.Create();
end;
function makeTrie(nodes: NodeArray): Trie;
begin
  Result.nodes := nodes;
end;
function makeNode(children: specialize TFPGMap<string, integer>; is_leaf: boolean): Node;
begin
  Result.children := children;
  Result.is_leaf := is_leaf;
end;
function new_trie(): Trie;
begin
  exit(makeTrie([makeNode(Map1(), false)]));
end;
function remove_key(m: specialize TFPGMap<string, integer>; k: string): specialize TFPGMap<string, integer>;
var
  remove_key_out: specialize TFPGMap<string, integer>;
  remove_key_key: string;
begin
  remove_key_out := specialize TFPGMap<string, integer>.Create();
  for remove_key_key_idx := 0 to (m.Count - 1) do begin
  remove_key_key := m.Keys[remove_key_key_idx];
  if remove_key_key <> k then begin
  remove_key_out[remove_key_key] := m[remove_key_key];
end;
end;
  exit(remove_key_out);
end;
procedure insert(trie_var: Trie; word: string);
var
  insert_nodes: array of Node;
  insert_curr: integer;
  insert_i: integer;
  insert_ch: string;
  insert_child_idx: integer;
  insert_children: specialize TFPGMap<string, integer>;
  insert_new_node: Node;
  insert_new_children: specialize TFPGMap<string, integer>;
  insert_node_var: Node;
begin
  insert_nodes := trie_var.nodes;
  insert_curr := 0;
  insert_i := 0;
  while insert_i < Length(word) do begin
  insert_ch := word[insert_i+1];
  insert_child_idx := -1;
  insert_children := insert_nodes[insert_curr].children;
  if insert_children.IndexOf(insert_ch) <> -1 then begin
  insert_child_idx := insert_children[insert_ch];
end else begin
  insert_new_node := makeNode(Map1(), false);
  insert_nodes := concat(insert_nodes, [insert_new_node]);
  insert_child_idx := Length(insert_nodes) - 1;
  insert_new_children := insert_children;
  insert_new_children[insert_ch] := insert_child_idx;
  insert_node_var := insert_nodes[insert_curr];
  insert_node_var.children := insert_new_children;
  insert_nodes[insert_curr] := insert_node_var;
end;
  insert_curr := insert_child_idx;
  insert_i := insert_i + 1;
end;
  insert_node_var := insert_nodes[insert_curr];
  insert_node_var.is_leaf := true;
  insert_nodes[insert_curr] := insert_node_var;
  trie_var.nodes := insert_nodes;
end;
procedure insert_many(trie_var: Trie; words: StrArray);
var
  insert_many_w: string;
begin
  for insert_many_w in words do begin
  insert(trie_var, insert_many_w);
end;
end;
function find(trie_var: Trie; word: string): boolean;
var
  find_nodes: array of Node;
  find_curr: integer;
  find_i: integer;
  find_ch: string;
  find_children: specialize TFPGMap<string, integer>;
  find_node_var: Node;
begin
  find_nodes := trie_var.nodes;
  find_curr := 0;
  find_i := 0;
  while find_i < Length(word) do begin
  find_ch := word[find_i+1];
  find_children := find_nodes[find_curr].children;
  if not find_children.IndexOf(find_ch) <> -1 then begin
  exit(false);
end;
  find_curr := find_children[find_ch];
  find_i := find_i + 1;
end;
  find_node_var := find_nodes[find_curr];
  exit(find_node_var.is_leaf);
end;
procedure delete(trie_var: Trie; word: string);
var
  delete_nodes: array of Node;
  delete_idx: integer;
  delete_pos: integer;
  function _delete(delete_idx: integer; delete_pos: integer): boolean;
begin
  if delete_pos = Length(word) then begin
  _delete_node_var := delete_nodes[delete_idx];
  if _delete_node_var.is_leaf = false then begin
  exit(false);
end;
  _delete_node_var.is_leaf := false;
  delete_nodes[delete_idx] := _delete_node_var;
  exit(Length(_delete_node_var.children) = 0);
end;
  _delete_node_var := delete_nodes[delete_idx];
  _delete_children := _delete_node_var.children;
  _delete_ch := word[delete_pos+1];
  if not _delete_children.IndexOf(_delete_ch) <> -1 then begin
  exit(false);
end;
  _delete_child_idx_idx := _delete_children.IndexOf(_delete_ch);
  if _delete_child_idx_idx <> -1 then begin
  _delete_child_idx := _delete_children.Data[_delete_child_idx_idx];
end else begin
  _delete_child_idx := 0;
end;
  _delete_should_delete := _delete(_delete_child_idx, delete_pos + 1);
  _delete_node_var := delete_nodes[delete_idx];
  if _delete_should_delete then begin
  _delete_new_children := remove_key(_delete_node_var.children, _delete_ch);
  _delete_node_var.children := _delete_new_children;
  delete_nodes[delete_idx] := _delete_node_var;
  exit((Length(_delete_new_children) = 0) and (_delete_node_var.is_leaf = false));
end;
  delete_nodes[delete_idx] := _delete_node_var;
  exit(false);
end;
begin
  delete_nodes := trie_var.nodes;
  _delete(0, 0);
  trie_var.nodes := delete_nodes;
end;
procedure print_words(trie_var: Trie);
var
  print_words_idx: integer;
  procedure dfs(print_words_idx: integer; word: string);
begin
  dfs_node_var := trie_var.nodes[print_words_idx];
  if dfs_node_var.is_leaf then begin
  writeln(word);
end;
  for dfs_key_idx := 0 to (.Count - 1) do begin
  dfs_key := .Keys[dfs_key_idx];
  dfs(dfs_node_var.children[dfs_key], word + dfs_key);
end;
end;
begin
  dfs(0, '');
end;
function test_trie(): boolean;
var
  test_trie_words: array of string;
  test_trie_trie_var: Trie;
  test_trie_ok: boolean;
  test_trie_w: string;
  test_trie_t: boolean;
  test_trie_t2: boolean;
  test_trie_t3: boolean;
  test_trie_t4: boolean;
begin
  test_trie_words := ['banana', 'bananas', 'bandana', 'band', 'apple', 'all', 'beast'];
  test_trie_trie_var := new_trie();
  insert_many(test_trie_trie_var, test_trie_words);
  test_trie_ok := true;
  for test_trie_w in test_trie_words do begin
  test_trie_ok := test_trie_ok and find(test_trie_trie_var, test_trie_w);
end;
  test_trie_ok := test_trie_ok and find(test_trie_trie_var, 'banana');
  test_trie_t := find(test_trie_trie_var, 'bandanas');
  test_trie_ok := test_trie_ok and (test_trie_t = false);
  test_trie_t2 := find(test_trie_trie_var, 'apps');
  test_trie_ok := test_trie_ok and (test_trie_t2 = false);
  test_trie_ok := test_trie_ok and find(test_trie_trie_var, 'apple');
  test_trie_ok := test_trie_ok and find(test_trie_trie_var, 'all');
  delete(test_trie_trie_var, 'all');
  test_trie_t3 := find(test_trie_trie_var, 'all');
  test_trie_ok := test_trie_ok and (test_trie_t3 = false);
  delete(test_trie_trie_var, 'banana');
  test_trie_t4 := find(test_trie_trie_var, 'banana');
  test_trie_ok := test_trie_ok and (test_trie_t4 = false);
  test_trie_ok := test_trie_ok and find(test_trie_trie_var, 'bananas');
  exit(test_trie_ok);
end;
procedure print_results(msg: string; passes: boolean);
begin
  if passes then begin
  writeln(msg + ' works!');
end else begin
  writeln(msg + ' doesn''t work :(');
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  trie_var := new_trie();
  print_results('Testing trie functionality', test_trie());
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
