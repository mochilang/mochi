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
  n: integer;
function abs_int(n: integer): integer; forward;
function num_digits(n: integer): integer; forward;
function num_digits_fast(n: integer): integer; forward;
function num_digits_faster(n: integer): integer; forward;
procedure test_num_digits(); forward;
procedure main(); forward;
function abs_int(n: integer): integer;
begin
  if n < 0 then begin
  exit(-n);
end;
  exit(n);
end;
function num_digits(n: integer): integer;
var
  num_digits_x: integer;
  num_digits_digits: integer;
begin
  num_digits_x := abs_int(n);
  num_digits_digits := 1;
  while num_digits_x >= 10 do begin
  num_digits_x := num_digits_x div 10;
  num_digits_digits := num_digits_digits + 1;
end;
  exit(num_digits_digits);
end;
function num_digits_fast(n: integer): integer;
var
  num_digits_fast_x: integer;
  num_digits_fast_digits: integer;
  num_digits_fast_power: integer;
begin
  num_digits_fast_x := abs_int(n);
  num_digits_fast_digits := 1;
  num_digits_fast_power := 10;
  while num_digits_fast_x >= num_digits_fast_power do begin
  num_digits_fast_power := num_digits_fast_power * 10;
  num_digits_fast_digits := num_digits_fast_digits + 1;
end;
  exit(num_digits_fast_digits);
end;
function num_digits_faster(n: integer): integer;
var
  num_digits_faster_s: string;
begin
  num_digits_faster_s := IntToStr(abs_int(n));
  exit(Length(num_digits_faster_s));
end;
procedure test_num_digits();
begin
  if num_digits(12345) <> 5 then begin
  panic('num_digits 12345 failed');
end;
  if num_digits(123) <> 3 then begin
  panic('num_digits 123 failed');
end;
  if num_digits(0) <> 1 then begin
  panic('num_digits 0 failed');
end;
  if num_digits(-1) <> 1 then begin
  panic('num_digits -1 failed');
end;
  if num_digits(-123456) <> 6 then begin
  panic('num_digits -123456 failed');
end;
  if num_digits_fast(12345) <> 5 then begin
  panic('num_digits_fast 12345 failed');
end;
  if num_digits_fast(123) <> 3 then begin
  panic('num_digits_fast 123 failed');
end;
  if num_digits_fast(0) <> 1 then begin
  panic('num_digits_fast 0 failed');
end;
  if num_digits_fast(-1) <> 1 then begin
  panic('num_digits_fast -1 failed');
end;
  if num_digits_fast(-123456) <> 6 then begin
  panic('num_digits_fast -123456 failed');
end;
  if num_digits_faster(12345) <> 5 then begin
  panic('num_digits_faster 12345 failed');
end;
  if num_digits_faster(123) <> 3 then begin
  panic('num_digits_faster 123 failed');
end;
  if num_digits_faster(0) <> 1 then begin
  panic('num_digits_faster 0 failed');
end;
  if num_digits_faster(-1) <> 1 then begin
  panic('num_digits_faster -1 failed');
end;
  if num_digits_faster(-123456) <> 6 then begin
  panic('num_digits_faster -123456 failed');
end;
end;
procedure main();
begin
  test_num_digits();
  writeln(IntToStr(num_digits(12345)));
  writeln(IntToStr(num_digits_fast(12345)));
  writeln(IntToStr(num_digits_faster(12345)));
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
