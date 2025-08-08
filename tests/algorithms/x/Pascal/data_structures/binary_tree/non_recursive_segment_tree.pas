{$mode objfpc}
program Main;
uses SysUtils;
type FuncType1 = function(arg0: integer; arg1: integer): integer;
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
  arr1: array of integer;
  st1: IntArray;
  arr2: array of integer;
  st2: IntArray;
  arr3: array of integer;
  st3: IntArray;
  arr4: array of integer;
  n4: integer;
  st4: IntArray;
  n: integer;
  v: integer;
  arr: IntArray;
  st: IntArray;
  right: integer;
  combine: FuncType1;
  a: integer;
  b: integer;
  p: integer;
  left: integer;
function build(arr: IntArray; combine: FuncType1): IntArray; forward;
procedure update(st: IntArray; n: integer; combine: FuncType1; p: integer; v: integer); forward;
function query(st: IntArray; n: integer; combine: FuncType1; left: integer; right: integer): integer; forward;
function add(a: integer; b: integer): integer; forward;
function min_int(a: integer; b: integer): integer; forward;
function max_int(a: integer; b: integer): integer; forward;
function build(arr: IntArray; combine: FuncType1): IntArray;
var
  build_n: integer;
  build_st: array of integer;
  build_i: integer;
begin
  build_n := Length(arr);
  build_st := [];
  build_i := 0;
  while build_i < (2 * build_n) do begin
  build_st := concat(build_st, IntArray([0]));
  build_i := build_i + 1;
end;
  build_i := 0;
  while build_i < build_n do begin
  build_st[build_n + build_i] := arr[build_i];
  build_i := build_i + 1;
end;
  build_i := build_n - 1;
  while build_i > 0 do begin
  build_st[build_i] := combine(build_st[build_i * 2], build_st[(build_i * 2) + 1]);
  build_i := build_i - 1;
end;
  exit(build_st);
end;
procedure update(st: IntArray; n: integer; combine: FuncType1; p: integer; v: integer);
var
  update_idx: integer;
begin
  update_idx := p + n;
  st[update_idx] := v;
  while update_idx > 1 do begin
  update_idx := Trunc(update_idx div 2);
  st[update_idx] := combine(st[update_idx * 2], st[(update_idx * 2) + 1]);
end;
end;
function query(st: IntArray; n: integer; combine: FuncType1; left: integer; right: integer): integer;
var
  query_l: integer;
  query_r: integer;
  query_res: integer;
  query_has: boolean;
begin
  query_l := left + n;
  query_r := right + n;
  query_res := 0;
  query_has := false;
  while query_l <= query_r do begin
  if (query_l mod 2) = 1 then begin
  if not query_has then begin
  query_res := st[query_l];
  query_has := true;
end else begin
  query_res := combine(query_res, st[query_l]);
end;
  query_l := query_l + 1;
end;
  if (query_r mod 2) = 0 then begin
  if not query_has then begin
  query_res := st[query_r];
  query_has := true;
end else begin
  query_res := combine(query_res, st[query_r]);
end;
  query_r := query_r - 1;
end;
  query_l := Trunc(query_l div 2);
  query_r := Trunc(query_r div 2);
end;
  exit(query_res);
end;
function add(a: integer; b: integer): integer;
begin
  exit(a + b);
end;
function min_int(a: integer; b: integer): integer;
begin
  if a < b then begin
  exit(a);
end else begin
  exit(b);
end;
end;
function max_int(a: integer; b: integer): integer;
begin
  if a > b then begin
  exit(a);
end else begin
  exit(b);
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  arr1 := [1, 2, 3];
  st1 := build(arr1, @add);
  writeln(IntToStr(query(st1, Length(arr1), @add, 0, 2)));
  arr2 := [3, 1, 2];
  st2 := build(arr2, @min_int);
  writeln(IntToStr(query(st2, Length(arr2), @min_int, 0, 2)));
  arr3 := [2, 3, 1];
  st3 := build(arr3, @max_int);
  writeln(IntToStr(query(st3, Length(arr3), @max_int, 0, 2)));
  arr4 := [1, 5, 7, -1, 6];
  n4 := Length(arr4);
  st4 := build(arr4, @add);
  update(st4, n4, @add, 1, -1);
  update(st4, n4, @add, 2, 3);
  writeln(IntToStr(query(st4, n4, @add, 1, 2)));
  writeln(IntToStr(query(st4, n4, @add, 1, 1)));
  update(st4, n4, @add, 4, 1);
  writeln(IntToStr(query(st4, n4, @add, 3, 4)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
