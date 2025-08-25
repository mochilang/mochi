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
procedure json(x: int64);
begin
  writeln(x);
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
function copy_list(copy_list_xs: IntArray): IntArray; forward;
function longest_subsequence(longest_subsequence_arr: IntArray): IntArray; forward;
procedure main(); forward;
function copy_list(copy_list_xs: IntArray): IntArray;
var
  copy_list_res: array of int64;
  copy_list_i: int64;
begin
  copy_list_res := [];
  copy_list_i := 0;
  while copy_list_i < Length(copy_list_xs) do begin
  copy_list_res := concat(copy_list_res, IntArray([copy_list_xs[copy_list_i]]));
  copy_list_i := copy_list_i + 1;
end;
  exit(copy_list_res);
end;
function longest_subsequence(longest_subsequence_arr: IntArray): IntArray;
var
  longest_subsequence_n: integer;
  longest_subsequence_lis: array of IntArray;
  longest_subsequence_i: int64;
  longest_subsequence_single: array of int64;
  longest_subsequence_prev: int64;
  longest_subsequence_temp: IntArray;
  longest_subsequence_temp2: array of int64;
  longest_subsequence_result_: array of int64;
begin
  longest_subsequence_n := Length(longest_subsequence_arr);
  longest_subsequence_lis := [];
  longest_subsequence_i := 0;
  while longest_subsequence_i < longest_subsequence_n do begin
  longest_subsequence_single := [];
  longest_subsequence_single := concat(longest_subsequence_single, IntArray([longest_subsequence_arr[longest_subsequence_i]]));
  longest_subsequence_lis := concat(longest_subsequence_lis, [longest_subsequence_single]);
  longest_subsequence_i := longest_subsequence_i + 1;
end;
  longest_subsequence_i := 1;
  while longest_subsequence_i < longest_subsequence_n do begin
  longest_subsequence_prev := 0;
  while longest_subsequence_prev < longest_subsequence_i do begin
  if (longest_subsequence_arr[longest_subsequence_prev] <= longest_subsequence_arr[longest_subsequence_i]) and ((Length(longest_subsequence_lis[longest_subsequence_prev]) + 1) > Length(longest_subsequence_lis[longest_subsequence_i])) then begin
  longest_subsequence_temp := copy_list(longest_subsequence_lis[longest_subsequence_prev]);
  longest_subsequence_temp2 := concat(longest_subsequence_temp, IntArray([longest_subsequence_arr[longest_subsequence_i]]));
  longest_subsequence_lis[longest_subsequence_i] := longest_subsequence_temp2;
end;
  longest_subsequence_prev := longest_subsequence_prev + 1;
end;
  longest_subsequence_i := longest_subsequence_i + 1;
end;
  longest_subsequence_result_ := [];
  longest_subsequence_i := 0;
  while longest_subsequence_i < longest_subsequence_n do begin
  if Length(longest_subsequence_lis[longest_subsequence_i]) > Length(longest_subsequence_result_) then begin
  longest_subsequence_result_ := longest_subsequence_lis[longest_subsequence_i];
end;
  longest_subsequence_i := longest_subsequence_i + 1;
end;
  exit(longest_subsequence_result_);
end;
procedure main();
begin
  writeln(list_int_to_str(longest_subsequence([10, 22, 9, 33, 21, 50, 41, 60, 80])));
  writeln(list_int_to_str(longest_subsequence([4, 8, 7, 5, 1, 12, 2, 3, 9])));
  writeln(list_int_to_str(longest_subsequence([9, 8, 7, 6, 5, 7])));
  writeln(list_int_to_str(longest_subsequence([28, 26, 12, 23, 35, 39])));
  writeln(list_int_to_str(longest_subsequence([1, 1, 1])));
  writeln(list_int_to_str(longest_subsequence(IntArray([]))));
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
