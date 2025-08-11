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
  right: integer;
  list_data: IntArray;
  key: integer;
  left: integer;
function search(list_data: IntArray; key: integer; left: integer; right: integer): integer; forward;
procedure main(); forward;
function search(list_data: IntArray; key: integer; left: integer; right: integer): integer;
var
  search_r: integer;
begin
  search_r := right;
  if search_r = 0 then begin
  search_r := Length(list_data) - 1;
end;
  if left > search_r then begin
  exit(-1);
end else begin
  if list_data[left] = key then begin
  exit(left);
end else begin
  if list_data[search_r] = key then begin
  exit(search_r);
end else begin
  exit(search(list_data, key, left + 1, search_r - 1));
end;
end;
end;
end;
procedure main();
begin
  writeln(search([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 5, 0, 0));
  writeln(search([1, 2, 4, 5, 3], 4, 0, 0));
  writeln(search([1, 2, 4, 5, 3], 6, 0, 0));
  writeln(search([5], 5, 0, 0));
  writeln(search([], 1, 0, 0));
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
