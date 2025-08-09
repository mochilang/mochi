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
  right: integer;
  key: integer;
  left: integer;
  v: IntArray;
function ceil_index(v: IntArray; left: integer; right: integer; key: integer): integer; forward;
function longest_increasing_subsequence_length(v: IntArray): integer; forward;
procedure main(); forward;
function ceil_index(v: IntArray; left: integer; right: integer; key: integer): integer;
var
  ceil_index_l: integer;
  ceil_index_r: integer;
  ceil_index_middle: integer;
begin
  ceil_index_l := left;
  ceil_index_r := right;
  while (ceil_index_r - ceil_index_l) > 1 do begin
  ceil_index_middle := (ceil_index_l + ceil_index_r) div 2;
  if v[ceil_index_middle] >= key then begin
  ceil_index_r := ceil_index_middle;
end else begin
  ceil_index_l := ceil_index_middle;
end;
end;
  exit(ceil_index_r);
end;
function longest_increasing_subsequence_length(v: IntArray): integer;
var
  longest_increasing_subsequence_length_tail: array of integer;
  longest_increasing_subsequence_length_i: integer;
  longest_increasing_subsequence_length_length_: integer;
  longest_increasing_subsequence_length_j: integer;
  longest_increasing_subsequence_length_idx: integer;
begin
  if Length(v) = 0 then begin
  exit(0);
end;
  longest_increasing_subsequence_length_tail := [];
  longest_increasing_subsequence_length_i := 0;
  while longest_increasing_subsequence_length_i < Length(v) do begin
  longest_increasing_subsequence_length_tail := concat(longest_increasing_subsequence_length_tail, IntArray([0]));
  longest_increasing_subsequence_length_i := longest_increasing_subsequence_length_i + 1;
end;
  longest_increasing_subsequence_length_length_ := 1;
  longest_increasing_subsequence_length_tail[0] := v[0];
  longest_increasing_subsequence_length_j := 1;
  while longest_increasing_subsequence_length_j < Length(v) do begin
  if v[longest_increasing_subsequence_length_j] < longest_increasing_subsequence_length_tail[0] then begin
  longest_increasing_subsequence_length_tail[0] := v[longest_increasing_subsequence_length_j];
end else begin
  if v[longest_increasing_subsequence_length_j] > longest_increasing_subsequence_length_tail[longest_increasing_subsequence_length_length_ - 1] then begin
  longest_increasing_subsequence_length_tail[longest_increasing_subsequence_length_length_] := v[longest_increasing_subsequence_length_j];
  longest_increasing_subsequence_length_length_ := longest_increasing_subsequence_length_length_ + 1;
end else begin
  longest_increasing_subsequence_length_idx := ceil_index(longest_increasing_subsequence_length_tail, -1, longest_increasing_subsequence_length_length_ - 1, v[longest_increasing_subsequence_length_j]);
  longest_increasing_subsequence_length_tail[longest_increasing_subsequence_length_idx] := v[longest_increasing_subsequence_length_j];
end;
end;
  longest_increasing_subsequence_length_j := longest_increasing_subsequence_length_j + 1;
end;
  exit(longest_increasing_subsequence_length_length_);
end;
procedure main();
var
  main_example1: array of integer;
  main_example2: array of integer;
  main_example3: array of integer;
  main_example4: array of integer;
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
end.
