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
function longest_subsequence(longest_subsequence_xs: IntArray): IntArray; forward;
function longest_subsequence(longest_subsequence_xs: IntArray): IntArray;
var
  longest_subsequence_n: integer;
  longest_subsequence_pivot: int64;
  longest_subsequence_is_found: boolean;
  longest_subsequence_i: int64;
  longest_subsequence_longest_subseq: array of int64;
  longest_subsequence_temp_array: array of int64;
  longest_subsequence_filtered: array of int64;
  longest_subsequence_j: int64;
  longest_subsequence_candidate: array of int64;
begin
  longest_subsequence_n := Length(longest_subsequence_xs);
  if longest_subsequence_n <= 1 then begin
  exit(longest_subsequence_xs);
end;
  longest_subsequence_pivot := longest_subsequence_xs[0];
  longest_subsequence_is_found := false;
  longest_subsequence_i := 1;
  longest_subsequence_longest_subseq := [];
  while not longest_subsequence_is_found and (longest_subsequence_i < longest_subsequence_n) do begin
  if longest_subsequence_xs[longest_subsequence_i] < longest_subsequence_pivot then begin
  longest_subsequence_is_found := true;
  longest_subsequence_temp_array := copy(longest_subsequence_xs, longest_subsequence_i, (longest_subsequence_n - (longest_subsequence_i)));
  longest_subsequence_temp_array := longest_subsequence(longest_subsequence_temp_array);
  if Length(longest_subsequence_temp_array) > Length(longest_subsequence_longest_subseq) then begin
  longest_subsequence_longest_subseq := longest_subsequence_temp_array;
end;
end else begin
  longest_subsequence_i := longest_subsequence_i + 1;
end;
end;
  longest_subsequence_filtered := [];
  longest_subsequence_j := 1;
  while longest_subsequence_j < longest_subsequence_n do begin
  if longest_subsequence_xs[longest_subsequence_j] >= longest_subsequence_pivot then begin
  longest_subsequence_filtered := concat(longest_subsequence_filtered, IntArray([longest_subsequence_xs[longest_subsequence_j]]));
end;
  longest_subsequence_j := longest_subsequence_j + 1;
end;
  longest_subsequence_candidate := [];
  longest_subsequence_candidate := concat(longest_subsequence_candidate, IntArray([longest_subsequence_pivot]));
  longest_subsequence_candidate := concat(longest_subsequence_candidate, longest_subsequence(longest_subsequence_filtered));
  if Length(longest_subsequence_candidate) > Length(longest_subsequence_longest_subseq) then begin
  exit(longest_subsequence_candidate);
end else begin
  exit(longest_subsequence_longest_subseq);
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
