{$mode objfpc}
program Main;
uses SysUtils;
type DS = record
  parent: array of integer;
  rank: array of integer;
end;
type FindResult = record
  ds: DS;
  root: integer;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  ds_var: DS;
  i: integer;
  j: integer;
  res_i: FindResult;
  root_i: integer;
  res_j: FindResult;
  root_j: integer;
  same: boolean;
  root_same: boolean;
  res: FindResult;
  b: integer;
  x: integer;
  a: integer;
  y: integer;
function makeFindResult(ds: DS; root: integer): FindResult; forward;
function makeDS(parent: IntArray; rank: IntArray): DS; forward;
function make_set(ds_var: DS; x: integer): DS; forward;
function find_set(ds_var: DS; x: integer): FindResult; forward;
function union_set(ds_var: DS; x: integer; y: integer): DS; forward;
function same_python_set(a: integer; b: integer): boolean; forward;
function makeFindResult(ds: DS; root: integer): FindResult;
begin
  Result.ds := ds;
  Result.root := root;
end;
function makeDS(parent: IntArray; rank: IntArray): DS;
begin
  Result.parent := parent;
  Result.rank := rank;
end;
function make_set(ds_var: DS; x: integer): DS;
var
  make_set_p: array of integer;
  make_set_r: array of integer;
begin
  make_set_p := ds_var.parent;
  make_set_r := ds_var.rank;
  make_set_p[x] := x;
  make_set_r[x] := 0;
  exit(makeDS(make_set_p, make_set_r));
end;
function find_set(ds_var: DS; x: integer): FindResult;
var
  find_set_res: FindResult;
  find_set_p: array of integer;
begin
  if ds_var.parent[x] = x then begin
  exit(makeFindResult(ds_var, x));
end;
  find_set_res := find_set(ds_var, ds_var.parent[x]);
  find_set_p := find_set_res.ds.parent;
  find_set_p[x] := find_set_res.root;
  exit(makeFindResult(makeDS(find_set_p, find_set_res.ds.rank), find_set_res.root));
end;
function union_set(ds_var: DS; x: integer; y: integer): DS;
var
  union_set_fx: FindResult;
  union_set_ds1: DS;
  union_set_x_root: integer;
  union_set_fy: FindResult;
  union_set_ds2: DS;
  union_set_y_root: integer;
  union_set_p: array of integer;
  union_set_r: array of integer;
begin
  union_set_fx := find_set(ds_var, x);
  union_set_ds1 := union_set_fx.ds;
  union_set_x_root := union_set_fx.root;
  union_set_fy := find_set(union_set_ds1, y);
  union_set_ds2 := union_set_fy.ds;
  union_set_y_root := union_set_fy.root;
  if union_set_x_root = union_set_y_root then begin
  exit(union_set_ds2);
end;
  union_set_p := union_set_ds2.parent;
  union_set_r := union_set_ds2.rank;
  if union_set_r[union_set_x_root] > union_set_r[union_set_y_root] then begin
  union_set_p[union_set_y_root] := union_set_x_root;
end else begin
  union_set_p[union_set_x_root] := union_set_y_root;
  if union_set_r[union_set_x_root] = union_set_r[union_set_y_root] then begin
  union_set_r[union_set_y_root] := union_set_r[union_set_y_root] + 1;
end;
end;
  exit(makeDS(union_set_p, union_set_r));
end;
function same_python_set(a: integer; b: integer): boolean;
begin
  if (a < 3) and (b < 3) then begin
  exit(true);
end;
  if (((a >= 3) and (a < 6)) and (b >= 3)) and (b < 6) then begin
  exit(true);
end;
  exit(false);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  ds_var := makeDS([], []);
  i := 0;
  while i < 6 do begin
  ds_var.parent := concat(ds_var.parent, IntArray([0]));
  ds_var.rank := concat(ds_var.rank, IntArray([0]));
  ds_var := make_set(ds_var, i);
  i := i + 1;
end;
  ds_var := union_set(ds_var, 0, 1);
  ds_var := union_set(ds_var, 1, 2);
  ds_var := union_set(ds_var, 3, 4);
  ds_var := union_set(ds_var, 3, 5);
  i := 0;
  while i < 6 do begin
  j := 0;
  while j < 6 do begin
  res_i := find_set(ds_var, i);
  ds_var := res_i.ds;
  root_i := res_i.root;
  res_j := find_set(ds_var, j);
  ds_var := res_j.ds;
  root_j := res_j.root;
  same := same_python_set(i, j);
  root_same := root_i = root_j;
  if same then begin
  if not root_same then begin
  panic('nodes should be in same set');
end;
end else begin
  if root_same then begin
  panic('nodes should be in different sets');
end;
end;
  j := j + 1;
end;
  i := i + 1;
end;
  i := 0;
  while i < 6 do begin
  res := find_set(ds_var, i);
  ds_var := res.ds;
  writeln(IntToStr(res.root));
  i := i + 1;
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
