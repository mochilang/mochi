{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
  num: integer;
function integer_square_root(num: integer): integer; forward;
procedure test_integer_square_root(); forward;
procedure main(); forward;
function integer_square_root(num: integer): integer;
var
  integer_square_root_left_bound: integer;
  integer_square_root_right_bound: integer;
  integer_square_root_mid: integer;
  integer_square_root_mid_squared: integer;
begin
  if num < 0 then begin
  panic('num must be non-negative integer');
end;
  if num < 2 then begin
  exit(num);
end;
  integer_square_root_left_bound := 0;
  integer_square_root_right_bound := num div 2;
  while integer_square_root_left_bound <= integer_square_root_right_bound do begin
  integer_square_root_mid := integer_square_root_left_bound + ((integer_square_root_right_bound - integer_square_root_left_bound) div 2);
  integer_square_root_mid_squared := integer_square_root_mid * integer_square_root_mid;
  if integer_square_root_mid_squared = num then begin
  exit(integer_square_root_mid);
end;
  if integer_square_root_mid_squared < num then begin
  integer_square_root_left_bound := integer_square_root_mid + 1;
end else begin
  integer_square_root_right_bound := integer_square_root_mid - 1;
end;
end;
  exit(integer_square_root_right_bound);
end;
procedure test_integer_square_root();
var
  test_integer_square_root_expected: array of integer;
  test_integer_square_root_i: integer;
  test_integer_square_root_result_: integer;
begin
  test_integer_square_root_expected := [0, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4];
  test_integer_square_root_i := 0;
  while test_integer_square_root_i < Length(test_integer_square_root_expected) do begin
  test_integer_square_root_result_ := integer_square_root(test_integer_square_root_i);
  if test_integer_square_root_result_ <> test_integer_square_root_expected[test_integer_square_root_i] then begin
  panic('test failed at index ' + IntToStr(test_integer_square_root_i));
end;
  test_integer_square_root_i := test_integer_square_root_i + 1;
end;
  if integer_square_root(625) <> 25 then begin
  panic('sqrt of 625 incorrect');
end;
  if integer_square_root(2147483647) <> 46340 then begin
  panic('sqrt of max int incorrect');
end;
end;
procedure main();
begin
  test_integer_square_root();
  writeln(IntToStr(integer_square_root(625)));
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
