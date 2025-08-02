{$mode objfpc}
program Main;
uses SysUtils, Variants, fgl;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function Map14(removeR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>; forward;
function Map13(removeR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>; forward;
function Map12(removeR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>; forward;
function Map11(removeR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>; forward;
function Map10(removeR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>; forward;
function Map9(dir: integer; root: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>; forward;
function Map8(dir: integer; root: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>; forward;
function Map7(dir: integer; root: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>; forward;
function Map6(insertR_dir: integer; insertR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>; forward;
function Map5(insertR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>; forward;
function Map4(insertR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>; forward;
function Map3(insertR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>; forward;
function Map2(data: integer): specialize TFPGMap<string, Variant>; forward;
function Map1(data: integer): specialize TFPGMap<string, Variant>; forward;
function Node(data: integer): specialize TFPGMap<string, Variant>; forward;
function getLink(n: specialize TFPGMap<string, Variant>; dir: integer): Variant; forward;
procedure setLink(n: specialize TFPGMap<string, Variant>; dir: integer; v: Variant); forward;
function opp(dir: integer): integer; forward;
function single(root: specialize TFPGMap<string, Variant>; dir: integer): specialize TFPGMap<string, Variant>; forward;
function double(root: specialize TFPGMap<string, Variant>; dir: integer): specialize TFPGMap<string, Variant>; forward;
procedure adjustBalance(root: specialize TFPGMap<string, Variant>; dir: integer; bal: integer); forward;
function insertBalance(root: specialize TFPGMap<string, Variant>; dir: integer): specialize TFPGMap<string, Variant>; forward;
function insertR(root: Variant; data: integer): specialize TFPGMap<string, Variant>; forward;
function Insert(tree: Variant; data: integer): Variant; forward;
function removeBalance(root: specialize TFPGMap<string, Variant>; dir: integer): specialize TFPGMap<string, Variant>; forward;
function removeR(root: Variant; data: integer): specialize TFPGMap<string, Variant>; forward;
function Remove(tree: Variant; data: integer): Variant; forward;
function indentStr(n: integer): string; forward;
procedure dumpNode(node: Variant; indent: integer; comma: boolean); forward;
procedure dump(node: Variant; indent: integer); forward;
procedure main(); forward;
function Map14(removeR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('node', Variant(removeR_node));
  Result.AddOrSetData('done', Variant(false));
end;
function Map13(removeR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('node', Variant(removeR_node));
  Result.AddOrSetData('done', Variant(true));
end;
function Map12(removeR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('node', Variant(removeR_node));
  Result.AddOrSetData('done', Variant(true));
end;
function Map11(removeR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('node', getLink(removeR_node, 0));
  Result.AddOrSetData('done', Variant(false));
end;
function Map10(removeR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('node', getLink(removeR_node, 1));
  Result.AddOrSetData('done', Variant(false));
end;
function Map9(dir: integer; root: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('node', Variant(single(root, dir)));
  Result.AddOrSetData('done', Variant(true));
end;
function Map8(dir: integer; root: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('node', Variant(double(root, dir)));
  Result.AddOrSetData('done', Variant(false));
end;
function Map7(dir: integer; root: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('node', Variant(single(root, dir)));
  Result.AddOrSetData('done', Variant(false));
end;
function Map6(insertR_dir: integer; insertR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('node', Variant(insertBalance(insertR_node, insertR_dir)));
  Result.AddOrSetData('done', Variant(true));
end;
function Map5(insertR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('node', Variant(insertR_node));
  Result.AddOrSetData('done', Variant(false));
end;
function Map4(insertR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('node', Variant(insertR_node));
  Result.AddOrSetData('done', Variant(true));
end;
function Map3(insertR_node: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('node', Variant(insertR_node));
  Result.AddOrSetData('done', Variant(true));
end;
function Map2(data: integer): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('node', Variant(Node(data)));
  Result.AddOrSetData('done', Variant(false));
end;
function Map1(data: integer): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('Data', Variant(data));
  Result.AddOrSetData('Balance', Variant(0));
  Result.AddOrSetData('Link', Variant([nil, nil]));
end;
function Node(data: integer): specialize TFPGMap<string, Variant>;
begin
  exit(Map1(data));
end;
function getLink(n: specialize TFPGMap<string, Variant>; dir: integer): Variant;
begin
  exit(n['Link'][dir]);
end;
procedure setLink(n: specialize TFPGMap<string, Variant>; dir: integer; v: Variant);
var
  setLink_links: array of Variant;
begin
  setLink_links := n['Link'];
  setLink_links[dir] := v;
  n.AddOrSetData('Link', Variant(setLink_links));
end;
function opp(dir: integer): integer;
begin
  exit(1 - dir);
end;
function single(root: specialize TFPGMap<string, Variant>; dir: integer): specialize TFPGMap<string, Variant>;
var
  single_tmp: Variant;
begin
  single_tmp := getLink(root, opp(dir));
  setLink(root, opp(dir), getLink(single_tmp, dir));
  setLink(single_tmp, dir, root);
  exit(single_tmp);
end;
function double(root: specialize TFPGMap<string, Variant>; dir: integer): specialize TFPGMap<string, Variant>;
var
  double_tmp: Variant;
begin
  double_tmp := getLink(getLink(root, opp(dir)), dir);
  setLink(getLink(root, opp(dir)), dir, getLink(double_tmp, opp(dir)));
  setLink(double_tmp, opp(dir), getLink(root, opp(dir)));
  setLink(root, opp(dir), double_tmp);
  double_tmp := getLink(root, opp(dir));
  setLink(root, opp(dir), getLink(double_tmp, dir));
  setLink(double_tmp, dir, root);
  exit(double_tmp);
end;
procedure adjustBalance(root: specialize TFPGMap<string, Variant>; dir: integer; bal: integer);
var
  adjustBalance_n: specialize TFPGMap<string, Variant>;
  adjustBalance_nn: specialize TFPGMap<string, Variant>;
begin
  adjustBalance_n := getLink(root, dir);
  adjustBalance_nn := getLink(adjustBalance_n, opp(dir));
  if adjustBalance_nn['Balance'] = 0 then begin
  root.AddOrSetData('Balance', Variant(0));
  adjustBalance_n.AddOrSetData('Balance', Variant(0));
end else begin
  if adjustBalance_nn['Balance'] = bal then begin
  root.AddOrSetData('Balance', Variant(-bal));
  adjustBalance_n.AddOrSetData('Balance', Variant(0));
end else begin
  root.AddOrSetData('Balance', Variant(0));
  adjustBalance_n.AddOrSetData('Balance', Variant(bal));
end;
end;
  adjustBalance_nn.AddOrSetData('Balance', Variant(0));
end;
function insertBalance(root: specialize TFPGMap<string, Variant>; dir: integer): specialize TFPGMap<string, Variant>;
var
  insertBalance_n: specialize TFPGMap<string, Variant>;
  insertBalance_bal: integer;
begin
  insertBalance_n := getLink(root, dir);
  insertBalance_bal := (2 * dir) - 1;
  if insertBalance_n['Balance'] = insertBalance_bal then begin
  root.AddOrSetData('Balance', Variant(0));
  insertBalance_n.AddOrSetData('Balance', Variant(0));
  exit(single(root, opp(dir)));
end;
  adjustBalance(root, dir, insertBalance_bal);
  exit(double(root, opp(dir)));
end;
function insertR(root: Variant; data: integer): specialize TFPGMap<string, Variant>;
var
  insertR_node: specialize TFPGMap<string, Variant>;
  insertR_dir: integer;
  insertR_r: integer;
begin
  if root = nil then begin
  exit(Map2(data));
end;
  insertR_node := root;
  insertR_dir := 0;
  if Trunc(insertR_node['Data']) < data then begin
  insertR_dir := 1;
end;
  insertR_r := insertR(getLink(insertR_node, insertR_dir), data);
  setLink(insertR_node, insertR_dir, insertR_r['node']);
  if insertR_r['done'] then begin
  exit(Map3(insertR_node));
end;
  insertR_node.AddOrSetData('Balance', Variant(Trunc(insertR_node['Balance']) + ((2 * insertR_dir) - 1)));
  if insertR_node['Balance'] = 0 then begin
  exit(Map4(insertR_node));
end;
  if (insertR_node['Balance'] = 1) or (insertR_node['Balance'] = -1) then begin
  exit(Map5(insertR_node));
end;
  exit(Map6(insertR_dir, insertR_node));
end;
function Insert(tree: Variant; data: integer): Variant;
var
  Insert_r: specialize TFPGMap<string, Variant>;
begin
  Insert_r := insertR(tree, data);
  exit(Insert_r['node']);
end;
function removeBalance(root: specialize TFPGMap<string, Variant>; dir: integer): specialize TFPGMap<string, Variant>;
var
  removeBalance_n: specialize TFPGMap<string, Variant>;
  removeBalance_bal: integer;
begin
  removeBalance_n := getLink(root, opp(dir));
  removeBalance_bal := (2 * dir) - 1;
  if removeBalance_n['Balance'] = -removeBalance_bal then begin
  root.AddOrSetData('Balance', Variant(0));
  removeBalance_n.AddOrSetData('Balance', Variant(0));
  exit(Map7(dir, root));
end;
  if removeBalance_n['Balance'] = removeBalance_bal then begin
  adjustBalance(root, opp(dir), -removeBalance_bal);
  exit(Map8(dir, root));
end;
  root.AddOrSetData('Balance', Variant(-removeBalance_bal));
  removeBalance_n.AddOrSetData('Balance', Variant(removeBalance_bal));
  exit(Map9(dir, root));
end;
function removeR(root: Variant; data: integer): specialize TFPGMap<string, Variant>;
var
  removeR_node: specialize TFPGMap<string, Variant>;
  removeR_heir: Variant;
  removeR_dir: integer;
  removeR_r: integer;
begin
  if root = nil then begin
  exit(Map2());
end;
  removeR_node := root;
  if Trunc(removeR_node['Data']) = data then begin
  if getLink(removeR_node, 0) = nil then begin
  exit(Map10(removeR_node));
end;
  if getLink(removeR_node, 1) = nil then begin
  exit(Map11(removeR_node));
end;
  removeR_heir := getLink(removeR_node, 0);
  while getLink(removeR_heir, 1) <> nil do begin
  removeR_heir := getLink(removeR_heir, 1);
end;
  removeR_node.AddOrSetData('Data', Variant(removeR_heir['Data']));
  data := Trunc(removeR_heir['Data']);
end;
  removeR_dir := 0;
  if Trunc(removeR_node['Data']) < data then begin
  removeR_dir := 1;
end;
  removeR_r := removeR(getLink(removeR_node, removeR_dir), data);
  setLink(removeR_node, removeR_dir, removeR_r['node']);
  if removeR_r['done'] then begin
  exit(Map12(removeR_node));
end;
  removeR_node.AddOrSetData('Balance', Variant((Trunc(removeR_node['Balance']) + 1) - (2 * removeR_dir)));
  if (removeR_node['Balance'] = 1) or (removeR_node['Balance'] = -1) then begin
  exit(Map13(removeR_node));
end;
  if removeR_node['Balance'] = 0 then begin
  exit(Map14(removeR_node));
end;
  exit(removeBalance(removeR_node, removeR_dir));
end;
function Remove(tree: Variant; data: integer): Variant;
var
  Remove_r: specialize TFPGMap<string, Variant>;
begin
  Remove_r := removeR(tree, data);
  exit(Remove_r['node']);
end;
function indentStr(n: integer): string;
var
  indentStr_s: string;
  indentStr_i: integer;
begin
  indentStr_s := '';
  indentStr_i := 0;
  while indentStr_i < n do begin
  indentStr_s := indentStr_s + ' ';
  indentStr_i := indentStr_i + 1;
end;
  exit(indentStr_s);
end;
procedure dumpNode(node: Variant; indent: integer; comma: boolean);
var
  dumpNode_sp: string;
  dumpNode_line: string;
  dumpNode_end: string;
begin
  dumpNode_sp := indentStr(indent);
  if node = nil then begin
  dumpNode_line := dumpNode_sp + 'null';
  if comma then begin
  dumpNode_line := dumpNode_line + ',';
end;
  writeln(dumpNode_line);
end else begin
  writeln(dumpNode_sp + '{');
  writeln(((indentStr(indent + 3) + '"Data": ') + IntToStr(node['Data'])) + ',');
  writeln(((indentStr(indent + 3) + '"Balance": ') + IntToStr(node['Balance'])) + ',');
  writeln(indentStr(indent + 3) + '"Link": [');
  dumpNode(getLink(node, 0), indent + 6, true);
  dumpNode(getLink(node, 1), indent + 6, false);
  writeln(indentStr(indent + 3) + ']');
  dumpNode_end := dumpNode_sp + '}';
  if comma then begin
  dumpNode_end := dumpNode_end + ',';
end;
  writeln(dumpNode_end);
end;
end;
procedure dump(node: Variant; indent: integer);
begin
  dumpNode(node, indent, false);
end;
procedure main();
var
  main_tree: Variant;
  main_t: specialize TFPGMap<string, Variant>;
begin
  main_tree := nil;
  writeln('Empty tree:');
  dump(main_tree, 0);
  writeln('');
  writeln('Insert test:');
  main_tree := Insert(main_tree, 3);
  main_tree := Insert(main_tree, 1);
  main_tree := Insert(main_tree, 4);
  main_tree := Insert(main_tree, 1);
  main_tree := Insert(main_tree, 5);
  dump(main_tree, 0);
  writeln('');
  writeln('Remove test:');
  main_tree := Remove(main_tree, 3);
  main_tree := Remove(main_tree, 1);
  main_t := main_tree;
  main_t.AddOrSetData('Balance', Variant(0));
  main_tree := main_t;
  dump(main_tree, 0);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
