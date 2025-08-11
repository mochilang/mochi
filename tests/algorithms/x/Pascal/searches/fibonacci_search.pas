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
  example1: array of integer;
  example2: array of integer;
  example3: array of integer;
  k: integer;
  a: integer;
  val: integer;
  b: integer;
  arr: IntArray;
function fibonacci(k: integer): integer; forward;
function min_int(a: integer; b: integer): integer; forward;
function fibonacci_search(arr: IntArray; val: integer): integer; forward;
function fibonacci(k: integer): integer;
var
  fibonacci_a: integer;
  fibonacci_b: integer;
  fibonacci_i: integer;
  fibonacci_tmp: integer;
begin
  if k < 0 then begin
  panic('k must be >= 0');
end;
  fibonacci_a := 0;
  fibonacci_b := 1;
  fibonacci_i := 0;
  while fibonacci_i < k do begin
  fibonacci_tmp := fibonacci_a + fibonacci_b;
  fibonacci_a := fibonacci_b;
  fibonacci_b := fibonacci_tmp;
  fibonacci_i := fibonacci_i + 1;
end;
  exit(fibonacci_a);
end;
function min_int(a: integer; b: integer): integer;
begin
  if a < b then begin
  exit(a);
end else begin
  exit(b);
end;
end;
function fibonacci_search(arr: IntArray; val: integer): integer;
var
  fibonacci_search_n: integer;
  fibonacci_search_m: integer;
  fibonacci_search_offset: integer;
  fibonacci_search_i: integer;
  fibonacci_search_item: integer;
begin
  fibonacci_search_n := Length(arr);
  fibonacci_search_m := 0;
  while fibonacci(fibonacci_search_m) < fibonacci_search_n do begin
  fibonacci_search_m := fibonacci_search_m + 1;
end;
  fibonacci_search_offset := 0;
  while fibonacci_search_m > 0 do begin
  fibonacci_search_i := min_int(fibonacci_search_offset + fibonacci(fibonacci_search_m - 1), fibonacci_search_n - 1);
  fibonacci_search_item := arr[fibonacci_search_i];
  if fibonacci_search_item = val then begin
  exit(fibonacci_search_i);
end else begin
  if val < fibonacci_search_item then begin
  fibonacci_search_m := fibonacci_search_m - 1;
end else begin
  fibonacci_search_offset := fibonacci_search_offset + fibonacci(fibonacci_search_m - 1);
  fibonacci_search_m := fibonacci_search_m - 2;
end;
end;
end;
  exit(-1);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  example1 := [4, 5, 6, 7];
  example2 := [-18, 2];
  example3 := [0, 5, 10, 15, 20, 25, 30];
  writeln(IntToStr(fibonacci_search(example1, 4)));
  writeln(IntToStr(fibonacci_search(example1, -10)));
  writeln(IntToStr(fibonacci_search(example2, -18)));
  writeln(IntToStr(fibonacci_search(example3, 15)));
  writeln(IntToStr(fibonacci_search(example3, 17)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
