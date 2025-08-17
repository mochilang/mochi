{$mode objfpc}{$modeswitch nestedprocvars}
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  colors: IntArray;
function sock_merchant(colors: IntArray): integer; forward;
procedure test_sock_merchant(); forward;
procedure main(); forward;
function sock_merchant(colors: IntArray): integer;
var
  sock_merchant_arr: array of integer;
  sock_merchant_i: integer;
  sock_merchant_n: integer;
  sock_merchant_a: integer;
  sock_merchant_min_idx: integer;
  sock_merchant_b: integer;
  sock_merchant_temp: integer;
  sock_merchant_pairs: integer;
  sock_merchant_count: integer;
begin
  sock_merchant_arr := [];
  sock_merchant_i := 0;
  while sock_merchant_i < Length(colors) do begin
  sock_merchant_arr := concat(sock_merchant_arr, IntArray([colors[sock_merchant_i]]));
  sock_merchant_i := sock_merchant_i + 1;
end;
  sock_merchant_n := Length(sock_merchant_arr);
  sock_merchant_a := 0;
  while sock_merchant_a < sock_merchant_n do begin
  sock_merchant_min_idx := sock_merchant_a;
  sock_merchant_b := sock_merchant_a + 1;
  while sock_merchant_b < sock_merchant_n do begin
  if sock_merchant_arr[sock_merchant_b] < sock_merchant_arr[sock_merchant_min_idx] then begin
  sock_merchant_min_idx := sock_merchant_b;
end;
  sock_merchant_b := sock_merchant_b + 1;
end;
  sock_merchant_temp := sock_merchant_arr[sock_merchant_a];
  sock_merchant_arr[sock_merchant_a] := sock_merchant_arr[sock_merchant_min_idx];
  sock_merchant_arr[sock_merchant_min_idx] := sock_merchant_temp;
  sock_merchant_a := sock_merchant_a + 1;
end;
  sock_merchant_pairs := 0;
  sock_merchant_i := 0;
  while sock_merchant_i < sock_merchant_n do begin
  sock_merchant_count := 1;
  while ((sock_merchant_i + 1) < sock_merchant_n) and (sock_merchant_arr[sock_merchant_i] = sock_merchant_arr[sock_merchant_i + 1]) do begin
  sock_merchant_count := sock_merchant_count + 1;
  sock_merchant_i := sock_merchant_i + 1;
end;
  sock_merchant_pairs := sock_merchant_pairs + (sock_merchant_count div 2);
  sock_merchant_i := sock_merchant_i + 1;
end;
  exit(sock_merchant_pairs);
end;
procedure test_sock_merchant();
var
  test_sock_merchant_example1: array of integer;
  test_sock_merchant_example2: array of integer;
begin
  test_sock_merchant_example1 := [10, 20, 20, 10, 10, 30, 50, 10, 20];
  if sock_merchant(test_sock_merchant_example1) <> 3 then begin
  panic('example1 failed');
end;
  test_sock_merchant_example2 := [1, 1, 3, 3];
  if sock_merchant(test_sock_merchant_example2) <> 2 then begin
  panic('example2 failed');
end;
end;
procedure main();
var
  main_example1: array of integer;
  main_example2: array of integer;
begin
  test_sock_merchant();
  main_example1 := [10, 20, 20, 10, 10, 30, 50, 10, 20];
  writeln(IntToStr(sock_merchant(main_example1)));
  main_example2 := [1, 1, 3, 3];
  writeln(IntToStr(sock_merchant(main_example2)));
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
