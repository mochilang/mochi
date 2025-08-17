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
  num: integer;
function remove_digit(num: integer): integer; forward;
procedure test_remove_digit(); forward;
procedure main(); forward;
function remove_digit(num: integer): integer;
var
  remove_digit_n: integer;
  remove_digit_max_val: integer;
  remove_digit_divisor: integer;
  remove_digit_higher: integer;
  remove_digit_lower: integer;
  remove_digit_candidate: integer;
begin
  remove_digit_n := num;
  if remove_digit_n < 0 then begin
  remove_digit_n := -remove_digit_n;
end;
  remove_digit_max_val := 0;
  remove_digit_divisor := 1;
  while remove_digit_divisor <= remove_digit_n do begin
  remove_digit_higher := remove_digit_n div (remove_digit_divisor * 10);
  remove_digit_lower := remove_digit_n mod remove_digit_divisor;
  remove_digit_candidate := (remove_digit_higher * remove_digit_divisor) + remove_digit_lower;
  if remove_digit_candidate > remove_digit_max_val then begin
  remove_digit_max_val := remove_digit_candidate;
end;
  remove_digit_divisor := remove_digit_divisor * 10;
end;
  exit(remove_digit_max_val);
end;
procedure test_remove_digit();
begin
  if remove_digit(152) <> 52 then begin
  panic('remove_digit(152) failed');
end;
  if remove_digit(6385) <> 685 then begin
  panic('remove_digit(6385) failed');
end;
  if remove_digit(-11) <> 1 then begin
  panic('remove_digit(-11) failed');
end;
  if remove_digit(2222222) <> 222222 then begin
  panic('remove_digit(2222222) failed');
end;
end;
procedure main();
begin
  test_remove_digit();
  writeln(remove_digit(152));
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
