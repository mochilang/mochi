{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function ceil_index(ceil_index_v: IntArray; ceil_index_left: int64; ceil_index_right: int64; ceil_index_key: int64): int64; forward;
function longest_increasing_subsequence_length(longest_increasing_subsequence_length_v: IntArray): int64; forward;
procedure main(); forward;
function ceil_index(ceil_index_v: IntArray; ceil_index_left: int64; ceil_index_right: int64; ceil_index_key: int64): int64;
var
  ceil_index_l: int64;
  ceil_index_r: int64;
  ceil_index_middle: int64;
begin
  ceil_index_l := ceil_index_left;
  ceil_index_r := ceil_index_right;
  while (ceil_index_r - ceil_index_l) > 1 do begin
  ceil_index_middle := _floordiv(ceil_index_l + ceil_index_r, 2);
  if ceil_index_v[ceil_index_middle] >= ceil_index_key then begin
  ceil_index_r := ceil_index_middle;
end else begin
  ceil_index_l := ceil_index_middle;
end;
end;
  exit(ceil_index_r);
end;
function longest_increasing_subsequence_length(longest_increasing_subsequence_length_v: IntArray): int64;
var
  longest_increasing_subsequence_length_tail: array of int64;
  longest_increasing_subsequence_length_i: int64;
  longest_increasing_subsequence_length_length_: int64;
  longest_increasing_subsequence_length_j: int64;
  longest_increasing_subsequence_length_idx: int64;
begin
  if Length(longest_increasing_subsequence_length_v) = 0 then begin
  exit(0);
end;
  longest_increasing_subsequence_length_tail := [];
  longest_increasing_subsequence_length_i := 0;
  while longest_increasing_subsequence_length_i < Length(longest_increasing_subsequence_length_v) do begin
  longest_increasing_subsequence_length_tail := concat(longest_increasing_subsequence_length_tail, IntArray([0]));
  longest_increasing_subsequence_length_i := longest_increasing_subsequence_length_i + 1;
end;
  longest_increasing_subsequence_length_length_ := 1;
  longest_increasing_subsequence_length_tail[0] := longest_increasing_subsequence_length_v[0];
  longest_increasing_subsequence_length_j := 1;
  while longest_increasing_subsequence_length_j < Length(longest_increasing_subsequence_length_v) do begin
  if longest_increasing_subsequence_length_v[longest_increasing_subsequence_length_j] < longest_increasing_subsequence_length_tail[0] then begin
  longest_increasing_subsequence_length_tail[0] := longest_increasing_subsequence_length_v[longest_increasing_subsequence_length_j];
end else begin
  if longest_increasing_subsequence_length_v[longest_increasing_subsequence_length_j] > longest_increasing_subsequence_length_tail[longest_increasing_subsequence_length_length_ - 1] then begin
  longest_increasing_subsequence_length_tail[longest_increasing_subsequence_length_length_] := longest_increasing_subsequence_length_v[longest_increasing_subsequence_length_j];
  longest_increasing_subsequence_length_length_ := longest_increasing_subsequence_length_length_ + 1;
end else begin
  longest_increasing_subsequence_length_idx := ceil_index(longest_increasing_subsequence_length_tail, -1, longest_increasing_subsequence_length_length_ - 1, longest_increasing_subsequence_length_v[longest_increasing_subsequence_length_j]);
  longest_increasing_subsequence_length_tail[longest_increasing_subsequence_length_idx] := longest_increasing_subsequence_length_v[longest_increasing_subsequence_length_j];
end;
end;
  longest_increasing_subsequence_length_j := longest_increasing_subsequence_length_j + 1;
end;
  exit(longest_increasing_subsequence_length_length_);
end;
procedure main();
var
  main_example1: array of int64;
  main_example2: array of int64;
  main_example3: array of int64;
  main_example4: array of int64;
begin
  main_example1 := [2, 5, 3, 7, 11, 8, 10, 13, 6];
  main_example2 := [];
  main_example3 := [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15];
  main_example4 := [5, 4, 3, 2, 1];
  writeln(longest_increasing_subsequence_length(main_example1));
  writeln(longest_increasing_subsequence_length(main_example2));
  writeln(longest_increasing_subsequence_length(main_example3));
  writeln(longest_increasing_subsequence_length(main_example4));
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
