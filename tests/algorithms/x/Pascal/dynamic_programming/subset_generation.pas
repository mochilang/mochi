{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
type IntArrayArray = array of IntArray;
type IntArrayArrayArray = array of IntArrayArray;
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
function copy_list(copy_list_src: IntArray): IntArray; forward;
function subset_combinations(subset_combinations_elements: IntArray; subset_combinations_n: int64): IntArrayArray; forward;
function copy_list(copy_list_src: IntArray): IntArray;
var
  copy_list_result_: array of int64;
  copy_list_i: int64;
begin
  copy_list_result_ := [];
  copy_list_i := 0;
  while copy_list_i < Length(copy_list_src) do begin
  copy_list_result_ := concat(copy_list_result_, IntArray([copy_list_src[copy_list_i]]));
  copy_list_i := copy_list_i + 1;
end;
  exit(copy_list_result_);
end;
function subset_combinations(subset_combinations_elements: IntArray; subset_combinations_n: int64): IntArrayArray;
var
  subset_combinations_r: integer;
  subset_combinations_dp: array of IntArrayArray;
  subset_combinations_i: int64;
  subset_combinations_j: int64;
  subset_combinations_prevs: array of IntArray;
  subset_combinations_k: int64;
  subset_combinations_prev: array of int64;
  subset_combinations_comb: IntArray;
begin
  subset_combinations_r := Length(subset_combinations_elements);
  if subset_combinations_n > subset_combinations_r then begin
  exit([]);
end;
  subset_combinations_dp := [];
  subset_combinations_i := 0;
  while subset_combinations_i <= subset_combinations_r do begin
  subset_combinations_dp := concat(subset_combinations_dp, [[]]);
  subset_combinations_i := subset_combinations_i + 1;
end;
  subset_combinations_dp[0] := concat(subset_combinations_dp[0], [[]]);
  subset_combinations_i := 1;
  while subset_combinations_i <= subset_combinations_r do begin
  subset_combinations_j := subset_combinations_i;
  while subset_combinations_j > 0 do begin
  subset_combinations_prevs := subset_combinations_dp[subset_combinations_j - 1];
  subset_combinations_k := 0;
  while subset_combinations_k < Length(subset_combinations_prevs) do begin
  subset_combinations_prev := subset_combinations_prevs[subset_combinations_k];
  subset_combinations_comb := copy_list(subset_combinations_prev);
  subset_combinations_comb := concat(subset_combinations_comb, IntArray([subset_combinations_elements[subset_combinations_i - 1]]));
  subset_combinations_dp[subset_combinations_j] := concat(subset_combinations_dp[subset_combinations_j], [subset_combinations_comb]);
  subset_combinations_k := subset_combinations_k + 1;
end;
  subset_combinations_j := subset_combinations_j - 1;
end;
  subset_combinations_i := subset_combinations_i + 1;
end;
  exit(subset_combinations_dp[subset_combinations_n]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_list_int_to_str(subset_combinations([10, 20, 30, 40], 2)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
