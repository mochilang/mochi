{$mode objfpc}
program Main;
uses SysUtils;
type PrefixSum = record
  prefix_sum: array of integer;
end;
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
  ps: PrefixSum;
  ps2: PrefixSum;
  arr: IntArray;
  start: integer;
  end_: integer;
  target_sum: integer;
function makePrefixSum(prefix_sum: IntArray): PrefixSum; forward;
function make_prefix_sum(arr: IntArray): PrefixSum; forward;
function get_sum(ps: PrefixSum; start: integer; end_: integer): integer; forward;
function contains_sum(ps: PrefixSum; target_sum: integer): boolean; forward;
function makePrefixSum(prefix_sum: IntArray): PrefixSum;
begin
  Result.prefix_sum := prefix_sum;
end;
function make_prefix_sum(arr: IntArray): PrefixSum;
var
  make_prefix_sum_prefix: array of integer;
  make_prefix_sum_running: integer;
  make_prefix_sum_i: integer;
begin
  make_prefix_sum_prefix := [];
  make_prefix_sum_running := 0;
  make_prefix_sum_i := 0;
  while make_prefix_sum_i < Length(arr) do begin
  make_prefix_sum_running := make_prefix_sum_running + arr[make_prefix_sum_i];
  make_prefix_sum_prefix := concat(make_prefix_sum_prefix, IntArray([make_prefix_sum_running]));
  make_prefix_sum_i := make_prefix_sum_i + 1;
end;
  exit(makePrefixSum(make_prefix_sum_prefix));
end;
function get_sum(ps: PrefixSum; start: integer; end_: integer): integer;
var
  get_sum_prefix: array of integer;
begin
  get_sum_prefix := ps.prefix_sum;
  if Length(get_sum_prefix) = 0 then begin
  panic('The array is empty.');
end;
  if ((start < 0) or (end_ >= Length(get_sum_prefix))) or (start > end_) then begin
  panic('Invalid range specified.');
end;
  if start = 0 then begin
  exit(get_sum_prefix[end_]);
end;
  exit(get_sum_prefix[end_] - get_sum_prefix[start - 1]);
end;
function contains_sum(ps: PrefixSum; target_sum: integer): boolean;
var
  contains_sum_prefix: array of integer;
  contains_sum_sums: array of integer;
  contains_sum_i: integer;
  contains_sum_sum_item: integer;
  contains_sum_j: integer;
begin
  contains_sum_prefix := ps.prefix_sum;
  contains_sum_sums := [0];
  contains_sum_i := 0;
  while contains_sum_i < Length(contains_sum_prefix) do begin
  contains_sum_sum_item := contains_sum_prefix[contains_sum_i];
  contains_sum_j := 0;
  while contains_sum_j < Length(contains_sum_sums) do begin
  if contains_sum_sums[contains_sum_j] = (contains_sum_sum_item - target_sum) then begin
  exit(true);
end;
  contains_sum_j := contains_sum_j + 1;
end;
  contains_sum_sums := concat(contains_sum_sums, IntArray([contains_sum_sum_item]));
  contains_sum_i := contains_sum_i + 1;
end;
  exit(false);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  ps := make_prefix_sum([1, 2, 3]);
  writeln(IntToStr(get_sum(ps, 0, 2)));
  writeln(IntToStr(get_sum(ps, 1, 2)));
  writeln(IntToStr(get_sum(ps, 2, 2)));
  writeln(LowerCase(BoolToStr(contains_sum(ps, 6), true)));
  writeln(LowerCase(BoolToStr(contains_sum(ps, 5), true)));
  writeln(LowerCase(BoolToStr(contains_sum(ps, 3), true)));
  writeln(LowerCase(BoolToStr(contains_sum(ps, 4), true)));
  writeln(LowerCase(BoolToStr(contains_sum(ps, 7), true)));
  ps2 := make_prefix_sum([1, -2, 3]);
  writeln(LowerCase(BoolToStr(contains_sum(ps2, 2), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
