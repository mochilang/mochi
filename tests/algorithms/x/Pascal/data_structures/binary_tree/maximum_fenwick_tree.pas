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
  arr: array of integer;
  left: integer;
  n: integer;
  value: integer;
  right: integer;
  idx: integer;
function zeros(n: integer): IntArray; forward;
procedure update(arr: IntArray; idx: integer; value: integer); forward;
function query(arr: IntArray; left: integer; right: integer): integer; forward;
function zeros(n: integer): IntArray;
var
  zeros_res: array of integer;
  zeros_i: integer;
begin
  zeros_res := [];
  zeros_i := 0;
  while zeros_i < n do begin
  zeros_res := concat(zeros_res, IntArray([0]));
  zeros_i := zeros_i + 1;
end;
  exit(zeros_res);
end;
procedure update(arr: IntArray; idx: integer; value: integer);
begin
  arr[idx] := value;
end;
function query(arr: IntArray; left: integer; right: integer): integer;
var
  query_result_: integer;
  query_i: integer;
begin
  query_result_ := 0;
  query_i := left;
  while query_i < right do begin
  if arr[query_i] > query_result_ then begin
  query_result_ := arr[query_i];
end;
  query_i := query_i + 1;
end;
  exit(query_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  arr := [0, 0, 0, 0, 0];
  writeln(query(arr, 0, 5));
  update(arr, 4, 100);
  writeln(query(arr, 0, 5));
  update(arr, 4, 0);
  update(arr, 2, 20);
  writeln(query(arr, 0, 5));
  update(arr, 4, 10);
  writeln(query(arr, 2, 5));
  writeln(query(arr, 1, 5));
  update(arr, 2, 0);
  writeln(query(arr, 0, 5));
  arr := zeros(10000);
  update(arr, 255, 30);
  writeln(query(arr, 0, 10000));
  arr := zeros(6);
  update(arr, 5, 1);
  writeln(query(arr, 5, 6));
  arr := zeros(6);
  update(arr, 0, 1000);
  writeln(query(arr, 0, 1));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
