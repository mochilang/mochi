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
procedure error(msg: string);
begin
  panic(msg);
end;
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function abs_int(abs_int_n: int64): int64; forward;
function sum_of_digits(sum_of_digits_n: int64): int64; forward;
function sum_of_digits_recursion(sum_of_digits_recursion_n: int64): int64; forward;
function sum_of_digits_compact(sum_of_digits_compact_n: int64): int64; forward;
procedure test_sum_of_digits(); forward;
procedure main(); forward;
function abs_int(abs_int_n: int64): int64;
begin
  if abs_int_n < 0 then begin
  exit(-abs_int_n);
end;
  exit(abs_int_n);
end;
function sum_of_digits(sum_of_digits_n: int64): int64;
var
  sum_of_digits_m: int64;
  sum_of_digits_res: int64;
begin
  sum_of_digits_m := abs_int(sum_of_digits_n);
  sum_of_digits_res := 0;
  while sum_of_digits_m > 0 do begin
  sum_of_digits_res := sum_of_digits_res + (sum_of_digits_m mod 10);
  sum_of_digits_m := _floordiv(sum_of_digits_m, 10);
end;
  exit(sum_of_digits_res);
end;
function sum_of_digits_recursion(sum_of_digits_recursion_n: int64): int64;
var
  sum_of_digits_recursion_m: int64;
begin
  sum_of_digits_recursion_m := abs_int(sum_of_digits_recursion_n);
  if sum_of_digits_recursion_m < 10 then begin
  exit(sum_of_digits_recursion_m);
end;
  exit((sum_of_digits_recursion_m mod 10) + sum_of_digits_recursion(_floordiv(sum_of_digits_recursion_m, 10)));
end;
function sum_of_digits_compact(sum_of_digits_compact_n: int64): int64;
var
  sum_of_digits_compact_s: string;
  sum_of_digits_compact_res: int64;
  sum_of_digits_compact_i: int64;
begin
  sum_of_digits_compact_s := IntToStr(abs_int(sum_of_digits_compact_n));
  sum_of_digits_compact_res := 0;
  sum_of_digits_compact_i := 0;
  while sum_of_digits_compact_i < Length(sum_of_digits_compact_s) do begin
  sum_of_digits_compact_res := sum_of_digits_compact_res + StrToInt(sum_of_digits_compact_s[sum_of_digits_compact_i+1]);
  sum_of_digits_compact_i := sum_of_digits_compact_i + 1;
end;
  exit(sum_of_digits_compact_res);
end;
procedure test_sum_of_digits();
begin
  if sum_of_digits(12345) <> 15 then begin
  panic('sum_of_digits 12345 failed');
end;
  if sum_of_digits(123) <> 6 then begin
  panic('sum_of_digits 123 failed');
end;
  if sum_of_digits(-123) <> 6 then begin
  panic('sum_of_digits -123 failed');
end;
  if sum_of_digits(0) <> 0 then begin
  panic('sum_of_digits 0 failed');
end;
  if sum_of_digits_recursion(12345) <> 15 then begin
  panic('recursion 12345 failed');
end;
  if sum_of_digits_recursion(123) <> 6 then begin
  panic('recursion 123 failed');
end;
  if sum_of_digits_recursion(-123) <> 6 then begin
  panic('recursion -123 failed');
end;
  if sum_of_digits_recursion(0) <> 0 then begin
  panic('recursion 0 failed');
end;
  if sum_of_digits_compact(12345) <> 15 then begin
  panic('compact 12345 failed');
end;
  if sum_of_digits_compact(123) <> 6 then begin
  panic('compact 123 failed');
end;
  if sum_of_digits_compact(-123) <> 6 then begin
  panic('compact -123 failed');
end;
  if sum_of_digits_compact(0) <> 0 then begin
  panic('compact 0 failed');
end;
end;
procedure main();
begin
  test_sum_of_digits();
  writeln(IntToStr(sum_of_digits(12345)));
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
  writeln('');
end.
