{$mode objfpc}
program Main;
uses SysUtils;
type TreeNode = record
end;
type BuildResult = record
  node: TreeNode;
  next: integer;
end;
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
  sep: string;
  idx: integer;
  data: string;
  s: string;
  nodes: StrArray;
  ch: string;
  node: TreeNode;
function makeBuildResult(node: TreeNode; next: integer): BuildResult; forward;
function makeTreeNode(): TreeNode; forward;
function digit(ch: string): integer; forward;
function to_int(s: string): integer; forward;
function split(s: string; sep: string): StrArray; forward;
function serialize(node: TreeNode): string; forward;
function build(nodes: StrArray; idx: integer): BuildResult; forward;
function deserialize(data: string): TreeNode; forward;
function five_tree(): TreeNode; forward;
procedure main(); forward;
function makeBuildResult(node: TreeNode; next: integer): BuildResult;
begin
  Result.node := node;
  Result.next := next;
end;
function makeTreeNode(): TreeNode;
begin
end;
function digit(ch: string): integer;
var
  digit_digits: string;
  digit_i: integer;
begin
  digit_digits := '0123456789';
  digit_i := 0;
  while digit_i < Length(digit_digits) do begin
  if copy(digit_digits, digit_i+1, (digit_i + 1 - (digit_i))) = ch then begin
  exit(digit_i);
end;
  digit_i := digit_i + 1;
end;
  exit(0);
end;
function to_int(s: string): integer;
var
  to_int_i: integer;
  to_int_sign: integer;
  to_int_num: integer;
  to_int_ch: string;
begin
  to_int_i := 0;
  to_int_sign := 1;
  if (Length(s) > 0) and (copy(s, 0+1, (1 - (0))) = '-') then begin
  to_int_sign := -1;
  to_int_i := 1;
end;
  to_int_num := 0;
  while to_int_i < Length(s) do begin
  to_int_ch := copy(s, to_int_i+1, (to_int_i + 1 - (to_int_i)));
  to_int_num := (to_int_num * 10) + digit(to_int_ch);
  to_int_i := to_int_i + 1;
end;
  exit(to_int_sign * to_int_num);
end;
function split(s: string; sep: string): StrArray;
var
  split_res: array of string;
  split_current: string;
  split_i: integer;
  split_ch: string;
begin
  split_res := [];
  split_current := '';
  split_i := 0;
  while split_i < Length(s) do begin
  split_ch := copy(s, split_i+1, (split_i + 1 - (split_i)));
  if split_ch = sep then begin
  split_res := concat(split_res, StrArray([split_current]));
  split_current := '';
end else begin
  split_current := split_current + split_ch;
end;
  split_i := split_i + 1;
end;
  split_res := concat(split_res, StrArray([split_current]));
  exit(split_res);
end;
function serialize(node: TreeNode): string;
begin
  exit(IfThen(node = Empty, 'null', (((IntToStr(v) + ',') + serialize(l)) + ',') + serialize(r)));
end;
function build(nodes: StrArray; idx: integer): BuildResult;
var
  build_value: string;
  build_left_res: BuildResult;
  build_right_res: BuildResult;
  build_node: Node;
begin
  build_value := nodes[idx];
  if build_value = 'null' then begin
  exit(makeBuildResult(makeEmpty(), idx + 1));
end;
  build_left_res := build(nodes, idx + 1);
  build_right_res := build(nodes, build_left_res.next);
  build_node := makeNode(build_left_res.node, to_int(build_value), build_right_res.node);
  exit(makeBuildResult(build_node, build_right_res.next));
end;
function deserialize(data: string): TreeNode;
var
  deserialize_nodes: StrArray;
  deserialize_res: BuildResult;
begin
  deserialize_nodes := split(data, ',');
  deserialize_res := build(deserialize_nodes, 0);
  exit(deserialize_res.node);
end;
function five_tree(): TreeNode;
var
  five_tree_left_child: Node;
  five_tree_right_left: Node;
  five_tree_right_right: Node;
  five_tree_right_child: Node;
begin
  five_tree_left_child := makeNode(2, makeEmpty(), makeEmpty());
  five_tree_right_left := makeNode(4, makeEmpty(), makeEmpty());
  five_tree_right_right := makeNode(5, makeEmpty(), makeEmpty());
  five_tree_right_child := makeNode(3, five_tree_right_left, five_tree_right_right);
  exit(makeNode(1, five_tree_left_child, five_tree_right_child));
end;
procedure main();
var
  main_root: TreeNode;
  main_serial: string;
  main_rebuilt: TreeNode;
  main_serial2: string;
begin
  main_root := five_tree();
  main_serial := serialize(main_root);
  writeln(main_serial);
  main_rebuilt := deserialize(main_serial);
  main_serial2 := serialize(main_rebuilt);
  writeln(main_serial2);
  writeln(Ord(main_serial = main_serial2));
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
