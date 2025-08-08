{$mode objfpc}
program Main;
uses SysUtils, fgl;
type SuffixTreeNode = record
  children: specialize TFPGMap<string, integer>;
  is_end_of_string: boolean;
  start: integer;
  end_: integer;
  suffix_link: integer;
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
  has_key_key_idx: integer;
  root: SuffixTreeNode;
  leaf: SuffixTreeNode;
  nodes: array of SuffixTreeNode;
  root_check: SuffixTreeNode;
  leaf_check: SuffixTreeNode;
  end_: integer;
  suffix_link: integer;
  children: specialize TFPGMap<string, integer>;
  start: integer;
  k: string;
  is_end_of_string: boolean;
  m: specialize TFPGMap<string, integer>;
function Map2(): specialize TFPGMap<string, integer>; forward;
function Map1(): specialize TFPGMap<string, integer>; forward;
function makeSuffixTreeNode(children: specialize TFPGMap<string, integer>; is_end_of_string: boolean; start: integer; end_: integer; suffix_link: integer): SuffixTreeNode; forward;
function new_suffix_tree_node(children: specialize TFPGMap<string, integer>; is_end_of_string: boolean; start: integer; end_: integer; suffix_link: integer): SuffixTreeNode; forward;
function empty_suffix_tree_node(): SuffixTreeNode; forward;
function has_key(m: specialize TFPGMap<string, integer>; k: string): boolean; forward;
function Map2(): specialize TFPGMap<string, integer>;
begin
  Result := specialize TFPGMap<string, integer>.Create();
  Result.AddOrSetData('a', Variant(1));
end;
function Map1(): specialize TFPGMap<string, integer>;
begin
  Result := specialize TFPGMap<string, integer>.Create();
end;
function makeSuffixTreeNode(children: specialize TFPGMap<string, integer>; is_end_of_string: boolean; start: integer; end_: integer; suffix_link: integer): SuffixTreeNode;
begin
  Result.children := children;
  Result.is_end_of_string := is_end_of_string;
  Result.start := start;
  Result.end_ := end_;
  Result.suffix_link := suffix_link;
end;
function new_suffix_tree_node(children: specialize TFPGMap<string, integer>; is_end_of_string: boolean; start: integer; end_: integer; suffix_link: integer): SuffixTreeNode;
begin
  exit(makeSuffixTreeNode(children, is_end_of_string, start, end_, suffix_link));
end;
function empty_suffix_tree_node(): SuffixTreeNode;
begin
  exit(new_suffix_tree_node(Map1(), false, 0 - 1, 0 - 1, 0 - 1));
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
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  root := new_suffix_tree_node(Map2(), false, 0 - 1, 0 - 1, 0 - 1);
  leaf := new_suffix_tree_node(Map1(), true, 0, 2, 0);
  nodes := [root, leaf];
  root_check := nodes[0];
  leaf_check := nodes[1];
  writeln(LowerCase(BoolToStr(has_key(root_check.children, 'a'), true)));
  writeln(LowerCase(BoolToStr(leaf_check.is_end_of_string, true)));
  writeln(IntToStr(leaf_check.start));
  writeln(IntToStr(leaf_check.end_));
  writeln(IntToStr(leaf_check.suffix_link));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
