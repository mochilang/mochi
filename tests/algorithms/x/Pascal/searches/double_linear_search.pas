{$mode objfpc}
program Main;
uses SysUtils;
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
  array_: IntArray;
  search_item: integer;
  n: integer;
function double_linear_search(array_: IntArray; search_item: integer): integer; forward;
procedure main(); forward;
function build_range(n: integer): IntArray; forward;
function double_linear_search(array_: IntArray; search_item: integer): integer;
var
  double_linear_search_start_ind: integer;
  double_linear_search_end_ind: integer;
begin
  double_linear_search_start_ind := 0;
  double_linear_search_end_ind := Length(array_) - 1;
  while double_linear_search_start_ind <= double_linear_search_end_ind do begin
  if array_[double_linear_search_start_ind] = search_item then begin
  exit(double_linear_search_start_ind);
end;
  if array_[double_linear_search_end_ind] = search_item then begin
  exit(double_linear_search_end_ind);
end;
  double_linear_search_start_ind := double_linear_search_start_ind + 1;
  double_linear_search_end_ind := double_linear_search_end_ind - 1;
end;
  exit(-1);
end;
procedure main();
var
  main_data: array of integer;
begin
  main_data := build_range(100);
  writeln(IntToStr(double_linear_search(main_data, 40)));
end;
function build_range(n: integer): IntArray;
var
  build_range_res: array of integer;
  build_range_i: integer;
begin
  build_range_res := [];
  build_range_i := 0;
  while build_range_i < n do begin
  build_range_res := concat(build_range_res, IntArray([build_range_i]));
  build_range_i := build_range_i + 1;
end;
  exit(build_range_res);
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
