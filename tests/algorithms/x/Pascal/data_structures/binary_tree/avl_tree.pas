{$mode objfpc}
program Main;
uses SysUtils, fgl;
type SpecializeTFPGMapStringIntegerArray = array of specialize TFPGMap<string, integer>;
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
  NIL_: integer;
  nodes: array of specialize TFPGMap<string, integer>;
  b: integer;
  a: integer;
  i: integer;
  value: integer;
function Map1(NIL_: integer; value: integer): specialize TFPGMap<string, integer>; forward;
function new_node(value: integer): integer; forward;
function get_height(i: integer): integer; forward;
function my_max(a: integer; b: integer): integer; forward;
procedure update_height(i: integer); forward;
function right_rotation(i: integer): integer; forward;
function left_rotation(i: integer): integer; forward;
function lr_rotation(i: integer): integer; forward;
function rl_rotation(i: integer): integer; forward;
function insert_node(i: integer; value: integer): integer; forward;
function get_left_most(i: integer): integer; forward;
function del_node(i: integer; value: integer): integer; forward;
function inorder(i: integer): string; forward;
procedure main(); forward;
function Map1(NIL_: integer; value: integer): specialize TFPGMap<string, integer>;
begin
  Result := specialize TFPGMap<string, integer>.Create();
  Result.AddOrSetData('data', Variant(value));
  Result.AddOrSetData('left', Variant(NIL_));
  Result.AddOrSetData('right', Variant(NIL_));
  Result.AddOrSetData('height', Variant(1));
end;
function new_node(value: integer): integer;
var
  new_node_node: specialize TFPGMap<string, integer>;
begin
  new_node_node := Map1(NIL_, value);
  nodes := concat(nodes, [new_node_node]);
  exit(Length(nodes) - 1);
end;
function get_height(i: integer): integer;
begin
  if i = NIL_ then begin
  exit(0);
end;
  exit(nodes[i]['height']);
end;
function my_max(a: integer; b: integer): integer;
begin
  if a > b then begin
  exit(a);
end;
  exit(b);
end;
procedure update_height(i: integer);
begin
  nodes[i]['height'] := my_max(get_height(nodes[i]['left']), get_height(nodes[i]['right'])) + 1;
end;
function right_rotation(i: integer): integer;
var
  right_rotation_left: integer;
  right_rotation_left_idx: integer;
  right_rotation_left_map: specialize TFPGMap<string, integer>;
begin
  right_rotation_left_map := nodes[i];
  right_rotation_left_idx := right_rotation_left_map.IndexOf('left');
  if right_rotation_left_idx <> -1 then begin
  right_rotation_left := right_rotation_left_map.Data[right_rotation_left_idx];
end else begin
  right_rotation_left := 0;
end;
  nodes[i]['left'] := nodes[right_rotation_left]['right'];
  nodes[right_rotation_left]['right'] := i;
  update_height(i);
  update_height(right_rotation_left);
  exit(right_rotation_left);
end;
function left_rotation(i: integer): integer;
var
  left_rotation_right: integer;
  left_rotation_right_idx: integer;
  left_rotation_right_map: specialize TFPGMap<string, integer>;
begin
  left_rotation_right_map := nodes[i];
  left_rotation_right_idx := left_rotation_right_map.IndexOf('right');
  if left_rotation_right_idx <> -1 then begin
  left_rotation_right := left_rotation_right_map.Data[left_rotation_right_idx];
end else begin
  left_rotation_right := 0;
end;
  nodes[i]['right'] := nodes[left_rotation_right]['left'];
  nodes[left_rotation_right]['left'] := i;
  update_height(i);
  update_height(left_rotation_right);
  exit(left_rotation_right);
end;
function lr_rotation(i: integer): integer;
begin
  nodes[i]['left'] := left_rotation(nodes[i]['left']);
  exit(right_rotation(i));
end;
function rl_rotation(i: integer): integer;
begin
  nodes[i]['right'] := right_rotation(nodes[i]['right']);
  exit(left_rotation(i));
end;
function insert_node(i: integer; value: integer): integer;
begin
  if i = NIL_ then begin
  exit(new_node(value));
end;
  if value < nodes[i]['data'] then begin
  nodes[i]['left'] := insert_node(nodes[i]['left'], value);
  if (get_height(nodes[i]['left']) - get_height(nodes[i]['right'])) = 2 then begin
  if value < nodes[nodes[i]['left']]['data'] then begin
  i := right_rotation(i);
end else begin
  i := lr_rotation(i);
end;
end;
end else begin
  nodes[i]['right'] := insert_node(nodes[i]['right'], value);
  if (get_height(nodes[i]['right']) - get_height(nodes[i]['left'])) = 2 then begin
  if value < nodes[nodes[i]['right']]['data'] then begin
  i := rl_rotation(i);
end else begin
  i := left_rotation(i);
end;
end;
end;
  update_height(i);
  exit(i);
end;
function get_left_most(i: integer): integer;
var
  get_left_most_cur: integer;
begin
  get_left_most_cur := i;
  while nodes[get_left_most_cur]['left'] <> NIL_ do begin
  get_left_most_cur := nodes[get_left_most_cur]['left'];
end;
  exit(nodes[get_left_most_cur]['data']);
end;
function del_node(i: integer; value: integer): integer;
var
  del_node_temp: integer;
  del_node_lh: integer;
  del_node_rh: integer;
begin
  if i = NIL_ then begin
  exit(NIL_);
end;
  if value < nodes[i]['data'] then begin
  nodes[i]['left'] := del_node(nodes[i]['left'], value);
end else begin
  if value > nodes[i]['data'] then begin
  nodes[i]['right'] := del_node(nodes[i]['right'], value);
end else begin
  if (nodes[i]['left'] <> NIL_) and (nodes[i]['right'] <> NIL_) then begin
  del_node_temp := get_left_most(nodes[i]['right']);
  nodes[i]['data'] := del_node_temp;
  nodes[i]['right'] := del_node(nodes[i]['right'], del_node_temp);
end else begin
  if nodes[i]['left'] <> NIL_ then begin
  i := nodes[i]['left'];
end else begin
  i := nodes[i]['right'];
end;
end;
end;
end;
  if i = NIL_ then begin
  exit(NIL_);
end;
  del_node_lh := get_height(nodes[i]['left']);
  del_node_rh := get_height(nodes[i]['right']);
  if (del_node_rh - del_node_lh) = 2 then begin
  if get_height(nodes[nodes[i]['right']]['right']) > get_height(nodes[nodes[i]['right']]['left']) then begin
  i := left_rotation(i);
end else begin
  i := rl_rotation(i);
end;
end else begin
  if (del_node_lh - del_node_rh) = 2 then begin
  if get_height(nodes[nodes[i]['left']]['left']) > get_height(nodes[nodes[i]['left']]['right']) then begin
  i := right_rotation(i);
end else begin
  i := lr_rotation(i);
end;
end;
end;
  update_height(i);
  exit(i);
end;
function inorder(i: integer): string;
var
  inorder_left: string;
  inorder_right: string;
  inorder_res: string;
begin
  if i = NIL_ then begin
  exit('');
end;
  inorder_left := inorder(nodes[i]['left']);
  inorder_right := inorder(nodes[i]['right']);
  inorder_res := IntToStr(nodes[i]['data']);
  if inorder_left <> '' then begin
  inorder_res := (inorder_left + ' ') + inorder_res;
end;
  if inorder_right <> '' then begin
  inorder_res := (inorder_res + ' ') + inorder_right;
end;
  exit(inorder_res);
end;
procedure main();
var
  main_root: integer;
begin
  nodes := [];
  main_root := NIL_;
  main_root := insert_node(main_root, 4);
  main_root := insert_node(main_root, 2);
  main_root := insert_node(main_root, 3);
  writeln(inorder(main_root));
  writeln(IntToStr(get_height(main_root)));
  main_root := del_node(main_root, 3);
  writeln(inorder(main_root));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  NIL_ := 0 - 1;
  nodes := [];
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
