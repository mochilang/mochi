{$mode objfpc}
program Main;
uses SysUtils;
type Tree = record
  values: array of integer;
  lefts: array of integer;
  rights: array of integer;
  root: integer;
end;
type Pair = record
  idx: integer;
  hd: integer;
end;
type IntArray = array of integer;
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
procedure show_list(xs: array of integer);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  NIL_: integer;
  tree_var: Tree;
  hds: IntArray;
  x: integer;
  vals: IntArray;
  xs: IntArray;
  t: Tree;
function makePair(idx: integer; hd: integer): Pair; forward;
function makeTree(values: IntArray; lefts: IntArray; rights: IntArray; root: integer): Tree; forward;
function make_tree(): Tree; forward;
function index_of(xs: IntArray; x: integer): integer; forward;
procedure sort_pairs(hds: IntArray; vals: IntArray); forward;
function right_view(t: Tree): IntArray; forward;
function left_view(t: Tree): IntArray; forward;
function top_view(t: Tree): IntArray; forward;
function bottom_view(t: Tree): IntArray; forward;
function makePair(idx: integer; hd: integer): Pair;
begin
  Result.idx := idx;
  Result.hd := hd;
end;
function makeTree(values: IntArray; lefts: IntArray; rights: IntArray; root: integer): Tree;
begin
  Result.values := values;
  Result.lefts := lefts;
  Result.rights := rights;
  Result.root := root;
end;
function make_tree(): Tree;
begin
  exit(makeTree([3, 9, 20, 15, 7], [1, NIL_, 3, NIL_, NIL_], [2, NIL_, 4, NIL_, NIL_], 0));
end;
function index_of(xs: IntArray; x: integer): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(xs) do begin
  if xs[index_of_i] = x then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(NIL_);
end;
procedure sort_pairs(hds: IntArray; vals: IntArray);
var
  sort_pairs_i: integer;
  sort_pairs_j: integer;
  sort_pairs_hd_tmp: integer;
  sort_pairs_val_tmp: integer;
begin
  sort_pairs_i := 0;
  while sort_pairs_i < Length(hds) do begin
  sort_pairs_j := sort_pairs_i;
  while (sort_pairs_j > 0) and (hds[sort_pairs_j - 1] > hds[sort_pairs_j]) do begin
  sort_pairs_hd_tmp := hds[sort_pairs_j - 1];
  hds[sort_pairs_j - 1] := hds[sort_pairs_j];
  hds[sort_pairs_j] := sort_pairs_hd_tmp;
  sort_pairs_val_tmp := vals[sort_pairs_j - 1];
  vals[sort_pairs_j - 1] := vals[sort_pairs_j];
  vals[sort_pairs_j] := sort_pairs_val_tmp;
  sort_pairs_j := sort_pairs_j - 1;
end;
  sort_pairs_i := sort_pairs_i + 1;
end;
end;
function right_view(t: Tree): IntArray;
var
  right_view_res: array of integer;
  right_view_queue: array of integer;
  right_view_size: integer;
  right_view_i: integer;
  right_view_idx: integer;
begin
  right_view_res := [];
  right_view_queue := [t.root];
  while Length(right_view_queue) > 0 do begin
  right_view_size := Length(right_view_queue);
  right_view_i := 0;
  while right_view_i < right_view_size do begin
  right_view_idx := right_view_queue[right_view_i];
  if t.lefts[right_view_idx] <> NIL_ then begin
  right_view_queue := concat(right_view_queue, IntArray([t.lefts[right_view_idx]]));
end;
  if t.rights[right_view_idx] <> NIL_ then begin
  right_view_queue := concat(right_view_queue, IntArray([t.rights[right_view_idx]]));
end;
  right_view_i := right_view_i + 1;
end;
  right_view_res := concat(right_view_res, IntArray([t.values[right_view_queue[right_view_size - 1]]]));
  right_view_queue := copy(right_view_queue, right_view_size, (Length(right_view_queue) - (right_view_size)));
end;
  exit(right_view_res);
end;
function left_view(t: Tree): IntArray;
var
  left_view_res: array of integer;
  left_view_queue: array of integer;
  left_view_size: integer;
  left_view_i: integer;
  left_view_idx: integer;
begin
  left_view_res := [];
  left_view_queue := [t.root];
  while Length(left_view_queue) > 0 do begin
  left_view_size := Length(left_view_queue);
  left_view_i := 0;
  while left_view_i < left_view_size do begin
  left_view_idx := left_view_queue[left_view_i];
  if t.lefts[left_view_idx] <> NIL_ then begin
  left_view_queue := concat(left_view_queue, IntArray([t.lefts[left_view_idx]]));
end;
  if t.rights[left_view_idx] <> NIL_ then begin
  left_view_queue := concat(left_view_queue, IntArray([t.rights[left_view_idx]]));
end;
  left_view_i := left_view_i + 1;
end;
  left_view_res := concat(left_view_res, IntArray([t.values[left_view_queue[0]]]));
  left_view_queue := copy(left_view_queue, left_view_size, (Length(left_view_queue) - (left_view_size)));
end;
  exit(left_view_res);
end;
function top_view(t: Tree): IntArray;
var
  top_view_hds: array of integer;
  top_view_vals: array of integer;
  top_view_queue_idx: array of integer;
  top_view_queue_hd: array of integer;
  top_view_idx: integer;
  top_view_hd: integer;
begin
  top_view_hds := [];
  top_view_vals := [];
  top_view_queue_idx := [t.root];
  top_view_queue_hd := [0];
  while Length(top_view_queue_idx) > 0 do begin
  top_view_idx := top_view_queue_idx[0];
  top_view_queue_idx := copy(top_view_queue_idx, 1, (Length(top_view_queue_idx) - (1)));
  top_view_hd := top_view_queue_hd[0];
  top_view_queue_hd := copy(top_view_queue_hd, 1, (Length(top_view_queue_hd) - (1)));
  if index_of(top_view_hds, top_view_hd) = NIL_ then begin
  top_view_hds := concat(top_view_hds, IntArray([top_view_hd]));
  top_view_vals := concat(top_view_vals, IntArray([t.values[top_view_idx]]));
end;
  if t.lefts[top_view_idx] <> NIL_ then begin
  top_view_queue_idx := concat(top_view_queue_idx, IntArray([t.lefts[top_view_idx]]));
  top_view_queue_hd := concat(top_view_queue_hd, IntArray([top_view_hd - 1]));
end;
  if t.rights[top_view_idx] <> NIL_ then begin
  top_view_queue_idx := concat(top_view_queue_idx, IntArray([t.rights[top_view_idx]]));
  top_view_queue_hd := concat(top_view_queue_hd, IntArray([top_view_hd + 1]));
end;
end;
  sort_pairs(top_view_hds, top_view_vals);
  exit(top_view_vals);
end;
function bottom_view(t: Tree): IntArray;
var
  bottom_view_hds: array of integer;
  bottom_view_vals: array of integer;
  bottom_view_queue_idx: array of integer;
  bottom_view_queue_hd: array of integer;
  bottom_view_idx: integer;
  bottom_view_hd: integer;
  bottom_view_pos: integer;
begin
  bottom_view_hds := [];
  bottom_view_vals := [];
  bottom_view_queue_idx := [t.root];
  bottom_view_queue_hd := [0];
  while Length(bottom_view_queue_idx) > 0 do begin
  bottom_view_idx := bottom_view_queue_idx[0];
  bottom_view_queue_idx := copy(bottom_view_queue_idx, 1, (Length(bottom_view_queue_idx) - (1)));
  bottom_view_hd := bottom_view_queue_hd[0];
  bottom_view_queue_hd := copy(bottom_view_queue_hd, 1, (Length(bottom_view_queue_hd) - (1)));
  bottom_view_pos := index_of(bottom_view_hds, bottom_view_hd);
  if bottom_view_pos = NIL_ then begin
  bottom_view_hds := concat(bottom_view_hds, IntArray([bottom_view_hd]));
  bottom_view_vals := concat(bottom_view_vals, IntArray([t.values[bottom_view_idx]]));
end else begin
  bottom_view_vals[bottom_view_pos] := t.values[bottom_view_idx];
end;
  if t.lefts[bottom_view_idx] <> NIL_ then begin
  bottom_view_queue_idx := concat(bottom_view_queue_idx, IntArray([t.lefts[bottom_view_idx]]));
  bottom_view_queue_hd := concat(bottom_view_queue_hd, IntArray([bottom_view_hd - 1]));
end;
  if t.rights[bottom_view_idx] <> NIL_ then begin
  bottom_view_queue_idx := concat(bottom_view_queue_idx, IntArray([t.rights[bottom_view_idx]]));
  bottom_view_queue_hd := concat(bottom_view_queue_hd, IntArray([bottom_view_hd + 1]));
end;
end;
  sort_pairs(bottom_view_hds, bottom_view_vals);
  exit(bottom_view_vals);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  NIL_ := 0 - 1;
  tree_var := make_tree();
  show_list(right_view(tree_var));
  show_list(left_view(tree_var));
  show_list(top_view(tree_var));
  show_list(bottom_view(tree_var));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
