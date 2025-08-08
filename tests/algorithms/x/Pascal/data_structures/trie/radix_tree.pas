{$mode objfpc}
program Main;
uses SysUtils, fgl;
type RadixNode = record
  prefix: string;
  is_leaf: boolean;
  children: specialize TFPGMap<string, integer>;
end;
type RadixTree = record
  nodes: array of RadixNode;
end;
type MatchResult = record
  common: string;
  rem_prefix: string;
  rem_word: string;
end;
type StrArray = array of string;
type RadixNodeArray = array of RadixNode;
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
function list_to_str(xs: array of string): string;
var i: integer;
begin
  Result := '#(' + sLineBreak;
  for i := 0 to High(xs) do begin
    Result := Result + '  ''' + xs[i] + '''.' + sLineBreak;
  end;
  Result := Result + ')';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  insert_child_idx_idx: integer;
  find_child_idx_idx: integer;
  remove_key_key_idx: integer;
  has_key_key_idx: integer;
  delete_child_idx_idx: integer;
  delete_k_idx: integer;
  delete_merge_idx_idx: integer;
  print_tree_child_idx_idx: integer;
  print_tree_k_idx: integer;
  node: RadixNode;
  height: integer;
  words: StrArray;
  prefix: string;
  m: specialize TFPGMap<string, integer>;
  is_leaf: boolean;
  word: string;
  k: string;
  idx: integer;
  tree: RadixTree;
function Map1(): specialize TFPGMap<string, integer>; forward;
function makeMatchResult(common: string; rem_prefix: string; rem_word: string): MatchResult; forward;
function makeRadixTree(nodes: RadixNodeArray): RadixTree; forward;
function makeRadixNode(prefix: string; is_leaf: boolean; children: specialize TFPGMap<string, integer>): RadixNode; forward;
function new_node(prefix: string; is_leaf: boolean): RadixNode; forward;
function new_tree(): RadixTree; forward;
function match_prefix(node: RadixNode; word: string): MatchResult; forward;
procedure insert_many(tree: RadixTree; words: StrArray); forward;
procedure insert(tree: RadixTree; idx: integer; word: string); forward;
function find(tree: RadixTree; idx: integer; word: string): boolean; forward;
function remove_key(m: specialize TFPGMap<string, integer>; k: string): specialize TFPGMap<string, integer>; forward;
function has_key(m: specialize TFPGMap<string, integer>; k: string): boolean; forward;
function delete(tree: RadixTree; idx: integer; word: string): boolean; forward;
procedure print_tree(tree: RadixTree; idx: integer; height: integer); forward;
function test_trie(): boolean; forward;
procedure pytests(); forward;
procedure main(); forward;
function Map1(): specialize TFPGMap<string, integer>;
begin
  Result := specialize TFPGMap<string, integer>.Create();
end;
function makeMatchResult(common: string; rem_prefix: string; rem_word: string): MatchResult;
begin
  Result.common := common;
  Result.rem_prefix := rem_prefix;
  Result.rem_word := rem_word;
end;
function makeRadixTree(nodes: RadixNodeArray): RadixTree;
begin
  Result.nodes := nodes;
end;
function makeRadixNode(prefix: string; is_leaf: boolean; children: specialize TFPGMap<string, integer>): RadixNode;
begin
  Result.prefix := prefix;
  Result.is_leaf := is_leaf;
  Result.children := children;
end;
function new_node(prefix: string; is_leaf: boolean): RadixNode;
begin
  exit(makeRadixNode(prefix, is_leaf, Map1()));
end;
function new_tree(): RadixTree;
var
  new_tree_nodes: array of RadixNode;
begin
  new_tree_nodes := [new_node('', false)];
  exit(makeRadixTree(new_tree_nodes));
end;
function match_prefix(node: RadixNode; word: string): MatchResult;
var
  match_prefix_x: integer;
  match_prefix_p: string;
  match_prefix_w: string;
  match_prefix_min_len: integer;
  match_prefix_common: string;
  match_prefix_rem_prefix: string;
  match_prefix_rem_word: string;
begin
  match_prefix_x := 0;
  match_prefix_p := node.prefix;
  match_prefix_w := word;
  match_prefix_min_len := Length(match_prefix_p);
  if Length(match_prefix_w) < match_prefix_min_len then begin
  match_prefix_min_len := Length(match_prefix_w);
end;
  while match_prefix_x < match_prefix_min_len do begin
  if copy(match_prefix_p, match_prefix_x+1, (match_prefix_x + 1 - (match_prefix_x))) <> copy(match_prefix_w, match_prefix_x+1, (match_prefix_x + 1 - (match_prefix_x))) then begin
  break;
end;
  match_prefix_x := match_prefix_x + 1;
end;
  match_prefix_common := copy(match_prefix_p, 0+1, (match_prefix_x - (0)));
  match_prefix_rem_prefix := copy(match_prefix_p, match_prefix_x+1, (Length(match_prefix_p) - (match_prefix_x)));
  match_prefix_rem_word := copy(match_prefix_w, match_prefix_x+1, (Length(match_prefix_w) - (match_prefix_x)));
  exit(makeMatchResult(match_prefix_common, match_prefix_rem_prefix, match_prefix_rem_word));
end;
procedure insert_many(tree: RadixTree; words: StrArray);
var
  insert_many_w: string;
begin
  for insert_many_w in words do begin
  insert(tree, 0, insert_many_w);
end;
end;
procedure insert(tree: RadixTree; idx: integer; word: string);
var
  insert_nodes: array of RadixNode;
  insert_node: RadixNode;
  insert_first: string;
  insert_children: specialize TFPGMap<string, integer>;
  insert_new_idx: integer;
  insert_child_idx: integer;
  insert_child: RadixNode;
  insert_res: MatchResult;
  insert_new_children: specialize TFPGMap<string, integer>;
begin
  insert_nodes := tree.nodes;
  insert_node := insert_nodes[idx];
  if (insert_node.prefix = word) and not insert_node.is_leaf then begin
  insert_node.is_leaf := true;
  insert_nodes[idx] := insert_node;
  tree.nodes := insert_nodes;
  exit();
end;
  insert_first := copy(word, 0+1, (1 - (0)));
  insert_children := insert_node.children;
  if not has_key(insert_children, insert_first) then begin
  insert_new_idx := Length(insert_nodes);
  insert_nodes := concat(insert_nodes, [new_node(word, true)]);
  insert_children[insert_first] := insert_new_idx;
  insert_node.children := insert_children;
  insert_nodes[idx] := insert_node;
  tree.nodes := insert_nodes;
  exit();
end;
  insert_child_idx_idx := insert_children.IndexOf(insert_first);
  if insert_child_idx_idx <> -1 then begin
  insert_child_idx := insert_children.Data[insert_child_idx_idx];
end else begin
  insert_child_idx := 0;
end;
  insert_child := insert_nodes[insert_child_idx];
  insert_res := match_prefix(insert_child, word);
  if insert_res.rem_prefix = '' then begin
  insert(tree, insert_child_idx, insert_res.rem_word);
  exit();
end;
  insert_child.prefix := insert_res.rem_prefix;
  insert_nodes[insert_child_idx] := insert_child;
  insert_new_children := specialize TFPGMap<string, integer>.Create();
  insert_new_children[copy(insert_res.rem_prefix, 0+1, (1 - (0)))] := insert_child_idx;
  insert_new_idx := Length(insert_nodes);
  insert_nodes := concat(insert_nodes, [new_node(insert_res.common, false)]);
  insert_nodes[insert_new_idx] := insert_new_children;
  if insert_res.rem_word = '' then begin
  insert_nodes[insert_new_idx] := true;
end else begin
  insert(tree, insert_new_idx, insert_res.rem_word);
end;
  insert_children[insert_first] := insert_new_idx;
  insert_node.children := insert_children;
  insert_nodes[idx] := insert_node;
  tree.nodes := insert_nodes;
end;
function find(tree: RadixTree; idx: integer; word: string): boolean;
var
  find_nodes: array of RadixNode;
  find_node: RadixNode;
  find_first: string;
  find_children: specialize TFPGMap<string, integer>;
  find_child_idx: integer;
  find_child: RadixNode;
  find_res: MatchResult;
begin
  find_nodes := tree.nodes;
  find_node := find_nodes[idx];
  find_first := copy(word, 0+1, (1 - (0)));
  find_children := find_node.children;
  if not has_key(find_children, find_first) then begin
  exit(false);
end;
  find_child_idx_idx := find_children.IndexOf(find_first);
  if find_child_idx_idx <> -1 then begin
  find_child_idx := find_children.Data[find_child_idx_idx];
end else begin
  find_child_idx := 0;
end;
  find_child := find_nodes[find_child_idx];
  find_res := match_prefix(find_child, word);
  if find_res.rem_prefix <> '' then begin
  exit(false);
end;
  if find_res.rem_word = '' then begin
  exit(find_child.is_leaf);
end;
  exit(find(tree, find_child_idx, find_res.rem_word));
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
function has_key(m: specialize TFPGMap<string, integer>; k: string): boolean;
var
  has_key_key: string;
begin
  for has_key_key_idx := 0 to (m.Count - 1) do begin
  has_key_key := m.Keys[has_key_key_idx];
  if has_key_key = k then begin
  exit(true);
end;
end;
  exit(false);
end;
function delete(tree: RadixTree; idx: integer; word: string): boolean;
var
  delete_nodes: array of RadixNode;
  delete_node: RadixNode;
  delete_first: string;
  delete_children: specialize TFPGMap<string, integer>;
  delete_child_idx: integer;
  delete_child: RadixNode;
  delete_res: MatchResult;
  delete_deleted: boolean;
  delete_only_key: string;
  delete_k: string;
  delete_merge_idx: integer;
  delete_merge_node: RadixNode;
begin
  delete_nodes := tree.nodes;
  delete_node := delete_nodes[idx];
  delete_first := copy(word, 0+1, (1 - (0)));
  delete_children := delete_node.children;
  if not has_key(delete_children, delete_first) then begin
  exit(false);
end;
  delete_child_idx_idx := delete_children.IndexOf(delete_first);
  if delete_child_idx_idx <> -1 then begin
  delete_child_idx := delete_children.Data[delete_child_idx_idx];
end else begin
  delete_child_idx := 0;
end;
  delete_child := delete_nodes[delete_child_idx];
  delete_res := match_prefix(delete_child, word);
  if delete_res.rem_prefix <> '' then begin
  exit(false);
end;
  if delete_res.rem_word <> '' then begin
  delete_deleted := delete(tree, delete_child_idx, delete_res.rem_word);
  if delete_deleted then begin
  delete_nodes := tree.nodes;
  delete_node := delete_nodes[idx];
end;
  exit(delete_deleted);
end;
  if not delete_child.is_leaf then begin
  exit(false);
end;
  if Length(delete_child.children) = 0 then begin
  delete_children := remove_key(delete_children, delete_first);
  delete_node.children := delete_children;
  delete_nodes[idx] := delete_node;
  tree.nodes := delete_nodes;
  if (Length(delete_children) = 1) and not delete_node.is_leaf then begin
  delete_only_key := '';
  for delete_k_idx := 0 to (delete_children.Count - 1) do begin
  delete_k := delete_children.Keys[delete_k_idx];
  delete_only_key := delete_k;
end;
  delete_merge_idx_idx := delete_children.IndexOf(delete_only_key);
  if delete_merge_idx_idx <> -1 then begin
  delete_merge_idx := delete_children.Data[delete_merge_idx_idx];
end else begin
  delete_merge_idx := 0;
end;
  delete_merge_node := delete_nodes[delete_merge_idx];
  delete_node.is_leaf := delete_merge_node.is_leaf;
  delete_node.prefix := delete_node.prefix + delete_merge_node.prefix;
  delete_node.children := delete_merge_node.children;
  delete_nodes[idx] := delete_node;
  tree.nodes := delete_nodes;
end;
end else begin
  if Length(delete_child.children) > 1 then begin
  delete_child.is_leaf := false;
  delete_nodes[delete_child_idx] := delete_child;
  tree.nodes := delete_nodes;
end else begin
  delete_only_key := '';
  for delete_k_idx := 0 to (.Count - 1) do begin
  delete_k := .Keys[delete_k_idx];
  delete_only_key := delete_k;
end;
  delete_merge_idx_idx := .IndexOf(delete_only_key);
  if delete_merge_idx_idx <> -1 then begin
  delete_merge_idx := .Data[delete_merge_idx_idx];
end else begin
  delete_merge_idx := 0;
end;
  delete_merge_node := delete_nodes[delete_merge_idx];
  delete_child.is_leaf := delete_merge_node.is_leaf;
  delete_child.prefix := delete_child.prefix + delete_merge_node.prefix;
  delete_child.children := delete_merge_node.children;
  delete_nodes[delete_child_idx] := delete_child;
  tree.nodes := delete_nodes;
end;
end;
  exit(true);
end;
procedure print_tree(tree: RadixTree; idx: integer; height: integer);
var
  print_tree_nodes: array of RadixNode;
  print_tree_node: RadixNode;
  print_tree_line: string;
  print_tree_i: integer;
  print_tree_children: specialize TFPGMap<string, integer>;
  print_tree_k: string;
  print_tree_child_idx: integer;
begin
  print_tree_nodes := tree.nodes;
  print_tree_node := print_tree_nodes[idx];
  if print_tree_node.prefix <> '' then begin
  print_tree_line := '';
  print_tree_i := 0;
  while print_tree_i < height do begin
  print_tree_line := print_tree_line + '-';
  print_tree_i := print_tree_i + 1;
end;
  print_tree_line := (print_tree_line + ' ') + print_tree_node.prefix;
  if print_tree_node.is_leaf then begin
  print_tree_line := print_tree_line + '  (leaf)';
end;
  writeln(print_tree_line);
end;
  print_tree_children := print_tree_node.children;
  for print_tree_k_idx := 0 to (print_tree_children.Count - 1) do begin
  print_tree_k := print_tree_children.Keys[print_tree_k_idx];
  print_tree_child_idx_idx := print_tree_children.IndexOf(print_tree_k);
  if print_tree_child_idx_idx <> -1 then begin
  print_tree_child_idx := print_tree_children.Data[print_tree_child_idx_idx];
end else begin
  print_tree_child_idx := 0;
end;
  print_tree(tree, print_tree_child_idx, height + 1);
end;
end;
function test_trie(): boolean;
var
  test_trie_words: array of string;
  test_trie_tree: RadixTree;
  test_trie_ok: boolean;
  test_trie_w: string;
begin
  test_trie_words := ['banana', 'bananas', 'bandana', 'band', 'apple', 'all', 'beast'];
  test_trie_tree := new_tree();
  insert_many(test_trie_tree, test_trie_words);
  test_trie_ok := true;
  for test_trie_w in test_trie_words do begin
  if not find(test_trie_tree, 0, test_trie_w) then begin
  test_trie_ok := false;
end;
end;
  if find(test_trie_tree, 0, 'bandanas') then begin
  test_trie_ok := false;
end;
  if find(test_trie_tree, 0, 'apps') then begin
  test_trie_ok := false;
end;
  delete(test_trie_tree, 0, 'all');
  if find(test_trie_tree, 0, 'all') then begin
  test_trie_ok := false;
end;
  delete(test_trie_tree, 0, 'banana');
  if find(test_trie_tree, 0, 'banana') then begin
  test_trie_ok := false;
end;
  if not find(test_trie_tree, 0, 'bananas') then begin
  test_trie_ok := false;
end;
  exit(test_trie_ok);
end;
procedure pytests();
begin
  if not test_trie() then begin
  panic('test failed');
end;
end;
procedure main();
var
  main_tree: RadixTree;
  main_words: array of string;
begin
  main_tree := new_tree();
  main_words := ['banana', 'bananas', 'bandanas', 'bandana', 'band', 'apple', 'all', 'beast'];
  insert_many(main_tree, main_words);
  writeln('Words: ' + list_to_str(main_words));
  writeln('Tree:');
  print_tree(main_tree, 0, 0);
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
