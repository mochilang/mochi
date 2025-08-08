{$mode objfpc}
program Main;
uses SysUtils, fgl;
type Node = record
  children: specialize TFPGMap<string, integer>;
  is_end_of_string: boolean;
  start: integer;
  end_: integer;
end;
type SuffixTree = record
  text: string;
  nodes: array of Node;
end;
type NodeArray = array of Node;
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
  has_key_key_idx: integer;
  st: SuffixTree;
  index: integer;
  text: string;
  m: specialize TFPGMap<string, integer>;
  k: string;
  tree: SuffixTree;
  pattern: string;
  suffix: string;
function Map1(): specialize TFPGMap<string, integer>; forward;
function makeSuffixTree(text: string; nodes: NodeArray): SuffixTree; forward;
function makeNode(children: specialize TFPGMap<string, integer>; is_end_of_string: boolean; start: integer; end_: integer): Node; forward;
function new_node(): Node; forward;
function has_key(m: specialize TFPGMap<string, integer>; k: string): boolean; forward;
function add_suffix(tree: SuffixTree; suffix: string; index: integer): SuffixTree; forward;
function build_suffix_tree(tree: SuffixTree): SuffixTree; forward;
function new_suffix_tree(text: string): SuffixTree; forward;
function search(tree: SuffixTree; pattern: string): boolean; forward;
function Map1(): specialize TFPGMap<string, integer>;
begin
  Result := specialize TFPGMap<string, integer>.Create();
end;
function makeSuffixTree(text: string; nodes: NodeArray): SuffixTree;
begin
  Result.text := text;
  Result.nodes := nodes;
end;
function makeNode(children: specialize TFPGMap<string, integer>; is_end_of_string: boolean; start: integer; end_: integer): Node;
begin
  Result.children := children;
  Result.is_end_of_string := is_end_of_string;
  Result.start := start;
  Result.end_ := end_;
end;
function new_node(): Node;
begin
  exit(makeNode(Map1(), false, -1, -1));
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
function add_suffix(tree: SuffixTree; suffix: string; index: integer): SuffixTree;
var
  add_suffix_nodes: array of Node;
  add_suffix_node_idx: integer;
  add_suffix_j: integer;
  add_suffix_ch: string;
  add_suffix_node_var: Node;
  add_suffix_children: specialize TFPGMap<string, integer>;
  add_suffix_new_idx: integer;
begin
  add_suffix_nodes := tree.nodes;
  add_suffix_node_idx := 0;
  add_suffix_j := 0;
  while add_suffix_j < Length(suffix) do begin
  add_suffix_ch := copy(suffix, add_suffix_j+1, (add_suffix_j + 1 - (add_suffix_j)));
  add_suffix_node_var := add_suffix_nodes[add_suffix_node_idx];
  add_suffix_children := add_suffix_node_var.children;
  if not has_key(add_suffix_children, add_suffix_ch) then begin
  add_suffix_nodes := concat(add_suffix_nodes, [new_node()]);
  add_suffix_new_idx := Length(add_suffix_nodes) - 1;
  add_suffix_children[add_suffix_ch] := add_suffix_new_idx;
end;
  add_suffix_node_var.children := add_suffix_children;
  add_suffix_nodes[add_suffix_node_idx] := add_suffix_node_var;
  add_suffix_node_idx := add_suffix_children[add_suffix_ch];
  add_suffix_j := add_suffix_j + 1;
end;
  add_suffix_node_var := add_suffix_nodes[add_suffix_node_idx];
  add_suffix_node_var.is_end_of_string := true;
  add_suffix_node_var.start := index;
  add_suffix_node_var.end_ := (index + Length(suffix)) - 1;
  add_suffix_nodes[add_suffix_node_idx] := add_suffix_node_var;
  tree.nodes := add_suffix_nodes;
  exit(tree);
end;
function build_suffix_tree(tree: SuffixTree): SuffixTree;
var
  build_suffix_tree_text: string;
  build_suffix_tree_n: integer;
  build_suffix_tree_i: integer;
  build_suffix_tree_t: SuffixTree;
  build_suffix_tree_suffix: string;
  build_suffix_tree_k: integer;
begin
  build_suffix_tree_text := tree.text;
  build_suffix_tree_n := Length(build_suffix_tree_text);
  build_suffix_tree_i := 0;
  build_suffix_tree_t := tree;
  while build_suffix_tree_i < build_suffix_tree_n do begin
  build_suffix_tree_suffix := '';
  build_suffix_tree_k := build_suffix_tree_i;
  while build_suffix_tree_k < build_suffix_tree_n do begin
  build_suffix_tree_suffix := build_suffix_tree_suffix + copy(build_suffix_tree_text, build_suffix_tree_k+1, (build_suffix_tree_k + 1 - (build_suffix_tree_k)));
  build_suffix_tree_k := build_suffix_tree_k + 1;
end;
  build_suffix_tree_t := add_suffix(build_suffix_tree_t, build_suffix_tree_suffix, build_suffix_tree_i);
  build_suffix_tree_i := build_suffix_tree_i + 1;
end;
  exit(build_suffix_tree_t);
end;
function new_suffix_tree(text: string): SuffixTree;
var
  new_suffix_tree_tree: SuffixTree;
begin
  new_suffix_tree_tree := makeSuffixTree(text, []);
  new_suffix_tree_tree.nodes := concat(new_suffix_tree_tree.nodes, [new_node()]);
  new_suffix_tree_tree := build_suffix_tree(new_suffix_tree_tree);
  exit(new_suffix_tree_tree);
end;
function search(tree: SuffixTree; pattern: string): boolean;
var
  search_node_idx: integer;
  search_i: integer;
  search_nodes: array of Node;
  search_ch: string;
  search_node_var: Node;
  search_children: specialize TFPGMap<string, integer>;
begin
  search_node_idx := 0;
  search_i := 0;
  search_nodes := tree.nodes;
  while search_i < Length(pattern) do begin
  search_ch := copy(pattern, search_i+1, (search_i + 1 - (search_i)));
  search_node_var := search_nodes[search_node_idx];
  search_children := search_node_var.children;
  if not has_key(search_children, search_ch) then begin
  exit(false);
end;
  search_node_idx := search_children[search_ch];
  search_i := search_i + 1;
end;
  exit(true);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  st := new_suffix_tree('bananas');
  writeln(LowerCase(BoolToStr(search(st, 'ana'), true)));
  writeln(LowerCase(BoolToStr(search(st, 'apple'), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
