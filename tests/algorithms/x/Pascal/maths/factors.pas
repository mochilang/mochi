{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  xs: IntArray;
  num: integer;
function reverse(xs: IntArray): IntArray; forward;
function factors_of_a_number(num: integer): IntArray; forward;
procedure run_tests(); forward;
procedure main(); forward;
function reverse(xs: IntArray): IntArray;
var
  reverse_res: array of integer;
  reverse_i: integer;
begin
  reverse_res := [];
  reverse_i := Length(xs) - 1;
  while reverse_i >= 0 do begin
  reverse_res := concat(reverse_res, IntArray([xs[reverse_i]]));
  reverse_i := reverse_i - 1;
end;
  exit(reverse_res);
end;
function factors_of_a_number(num: integer): IntArray;
var
  factors_of_a_number_facs: array of integer;
  factors_of_a_number_small: array of integer;
  factors_of_a_number_large: array of integer;
  factors_of_a_number_i: integer;
  factors_of_a_number_d: integer;
begin
  factors_of_a_number_facs := [];
  if num < 1 then begin
  exit(factors_of_a_number_facs);
end;
  factors_of_a_number_small := [];
  factors_of_a_number_large := [];
  factors_of_a_number_i := 1;
  while (factors_of_a_number_i * factors_of_a_number_i) <= num do begin
  if (num mod factors_of_a_number_i) = 0 then begin
  factors_of_a_number_small := concat(factors_of_a_number_small, IntArray([factors_of_a_number_i]));
  factors_of_a_number_d := num div factors_of_a_number_i;
  if factors_of_a_number_d <> factors_of_a_number_i then begin
  factors_of_a_number_large := concat(factors_of_a_number_large, IntArray([factors_of_a_number_d]));
end;
end;
  factors_of_a_number_i := factors_of_a_number_i + 1;
end;
  factors_of_a_number_facs := concat(factors_of_a_number_small, reverse(factors_of_a_number_large));
  exit(factors_of_a_number_facs);
end;
procedure run_tests();
begin
  if list_int_to_str(factors_of_a_number(1)) <> list_int_to_str([1]) then begin
  panic('case1 failed');
end;
  if list_int_to_str(factors_of_a_number(5)) <> list_int_to_str([1, 5]) then begin
  panic('case2 failed');
end;
  if list_int_to_str(factors_of_a_number(24)) <> list_int_to_str([1, 2, 3, 4, 6, 8, 12, 24]) then begin
  panic('case3 failed');
end;
  if list_int_to_str(factors_of_a_number(-24)) <> list_int_to_str([]) then begin
  panic('case4 failed');
end;
end;
procedure main();
begin
  run_tests();
  writeln(list_int_to_str(factors_of_a_number(24)));
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
