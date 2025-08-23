{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
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
function list_int_to_str(xs: array of int64): string;
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
function prefix_function(prefix_function_s: string): IntArray; forward;
function longest_prefix(longest_prefix_s: string): int64; forward;
function list_eq_int(list_eq_int_a: IntArray; list_eq_int_b: IntArray): boolean; forward;
procedure test_prefix_function(); forward;
procedure test_longest_prefix(); forward;
procedure main(); forward;
function prefix_function(prefix_function_s: string): IntArray;
var
  prefix_function_pi: array of int64;
  prefix_function_i: int64;
  prefix_function_j: int64;
begin
  prefix_function_pi := [];
  prefix_function_i := 0;
  while prefix_function_i < Length(prefix_function_s) do begin
  prefix_function_pi := concat(prefix_function_pi, IntArray([0]));
  prefix_function_i := prefix_function_i + 1;
end;
  prefix_function_i := 1;
  while prefix_function_i < Length(prefix_function_s) do begin
  prefix_function_j := prefix_function_pi[prefix_function_i - 1];
  while (prefix_function_j > 0) and (prefix_function_s[prefix_function_i+1] <> prefix_function_s[prefix_function_j+1]) do begin
  prefix_function_j := prefix_function_pi[prefix_function_j - 1];
end;
  if prefix_function_s[prefix_function_i+1] = prefix_function_s[prefix_function_j+1] then begin
  prefix_function_j := prefix_function_j + 1;
end;
  prefix_function_pi[prefix_function_i] := prefix_function_j;
  prefix_function_i := prefix_function_i + 1;
end;
  exit(prefix_function_pi);
end;
function longest_prefix(longest_prefix_s: string): int64;
var
  longest_prefix_pi: IntArray;
  longest_prefix_max_val: int64;
  longest_prefix_i: int64;
begin
  longest_prefix_pi := prefix_function(longest_prefix_s);
  longest_prefix_max_val := 0;
  longest_prefix_i := 0;
  while longest_prefix_i < Length(longest_prefix_pi) do begin
  if longest_prefix_pi[longest_prefix_i] > longest_prefix_max_val then begin
  longest_prefix_max_val := longest_prefix_pi[longest_prefix_i];
end;
  longest_prefix_i := longest_prefix_i + 1;
end;
  exit(longest_prefix_max_val);
end;
function list_eq_int(list_eq_int_a: IntArray; list_eq_int_b: IntArray): boolean;
var
  list_eq_int_i: int64;
begin
  if Length(list_eq_int_a) <> Length(list_eq_int_b) then begin
  exit(false);
end;
  list_eq_int_i := 0;
  while list_eq_int_i < Length(list_eq_int_a) do begin
  if list_eq_int_a[list_eq_int_i] <> list_eq_int_b[list_eq_int_i] then begin
  exit(false);
end;
  list_eq_int_i := list_eq_int_i + 1;
end;
  exit(true);
end;
procedure test_prefix_function();
var
  test_prefix_function_s1: string;
  test_prefix_function_expected1: array of int64;
  test_prefix_function_r1: IntArray;
  test_prefix_function_s2: string;
  test_prefix_function_expected2: array of int64;
  test_prefix_function_r2: IntArray;
begin
  test_prefix_function_s1 := 'aabcdaabc';
  test_prefix_function_expected1 := [0, 1, 0, 0, 0, 1, 2, 3, 4];
  test_prefix_function_r1 := prefix_function(test_prefix_function_s1);
  if not list_eq_int(test_prefix_function_r1, test_prefix_function_expected1) then begin
  panic('prefix_function aabcdaabc failed');
end;
  test_prefix_function_s2 := 'asdasdad';
  test_prefix_function_expected2 := [0, 0, 0, 1, 2, 3, 4, 0];
  test_prefix_function_r2 := prefix_function(test_prefix_function_s2);
  if not list_eq_int(test_prefix_function_r2, test_prefix_function_expected2) then begin
  panic('prefix_function asdasdad failed');
end;
end;
procedure test_longest_prefix();
begin
  if longest_prefix('aabcdaabc') <> 4 then begin
  panic('longest_prefix example1 failed');
end;
  if longest_prefix('asdasdad') <> 4 then begin
  panic('longest_prefix example2 failed');
end;
  if longest_prefix('abcab') <> 2 then begin
  panic('longest_prefix example3 failed');
end;
end;
procedure main();
var
  main_r1: IntArray;
  main_r2: IntArray;
begin
  test_prefix_function();
  test_longest_prefix();
  main_r1 := prefix_function('aabcdaabc');
  main_r2 := prefix_function('asdasdad');
  writeln(list_int_to_str(main_r1));
  writeln(list_int_to_str(main_r2));
  writeln(IntToStr(longest_prefix('aabcdaabc')));
  writeln(IntToStr(longest_prefix('abcab')));
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
