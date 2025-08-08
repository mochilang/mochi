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
  value: integer;
  array_: IntArray;
  n: integer;
  dp: IntArray;
  target: integer;
  len: integer;
function make_list(len: integer; value: integer): IntArray; forward;
function count_recursive(array_: IntArray; target: integer): integer; forward;
function combination_sum_iv(array_: IntArray; target: integer): integer; forward;
function count_dp(array_: IntArray; target: integer; dp: IntArray): integer; forward;
function combination_sum_iv_dp_array(array_: IntArray; target: integer): integer; forward;
function combination_sum_iv_bottom_up(n: integer; array_: IntArray; target: integer): integer; forward;
function make_list(len: integer; value: integer): IntArray;
var
  make_list_arr: array of integer;
  make_list_i: integer;
begin
  make_list_arr := [];
  make_list_i := 0;
  while make_list_i < len do begin
  make_list_arr := concat(make_list_arr, IntArray([value]));
  make_list_i := make_list_i + 1;
end;
  exit(make_list_arr);
end;
function count_recursive(array_: IntArray; target: integer): integer;
var
  count_recursive_total: integer;
  count_recursive_i: integer;
begin
  if target < 0 then begin
  exit(0);
end;
  if target = 0 then begin
  exit(1);
end;
  count_recursive_total := 0;
  count_recursive_i := 0;
  while count_recursive_i < Length(array_) do begin
  count_recursive_total := count_recursive_total + count_recursive(array_, target - array_[count_recursive_i]);
  count_recursive_i := count_recursive_i + 1;
end;
  exit(count_recursive_total);
end;
function combination_sum_iv(array_: IntArray; target: integer): integer;
begin
  exit(count_recursive(array_, target));
end;
function count_dp(array_: IntArray; target: integer; dp: IntArray): integer;
var
  count_dp_total: integer;
  count_dp_i: integer;
begin
  if target < 0 then begin
  exit(0);
end;
  if target = 0 then begin
  exit(1);
end;
  if dp[target] > (0 - 1) then begin
  exit(dp[target]);
end;
  count_dp_total := 0;
  count_dp_i := 0;
  while count_dp_i < Length(array_) do begin
  count_dp_total := count_dp_total + count_dp(array_, target - array_[count_dp_i], dp);
  count_dp_i := count_dp_i + 1;
end;
  dp[target] := count_dp_total;
  exit(count_dp_total);
end;
function combination_sum_iv_dp_array(array_: IntArray; target: integer): integer;
var
  combination_sum_iv_dp_array_dp: IntArray;
begin
  combination_sum_iv_dp_array_dp := make_list(target + 1, -1);
  exit(count_dp(array_, target, combination_sum_iv_dp_array_dp));
end;
function combination_sum_iv_bottom_up(n: integer; array_: IntArray; target: integer): integer;
var
  combination_sum_iv_bottom_up_dp: IntArray;
  combination_sum_iv_bottom_up_i: integer;
  combination_sum_iv_bottom_up_j: integer;
begin
  combination_sum_iv_bottom_up_dp := make_list(target + 1, 0);
  combination_sum_iv_bottom_up_dp[0] := 1;
  combination_sum_iv_bottom_up_i := 1;
  while combination_sum_iv_bottom_up_i <= target do begin
  combination_sum_iv_bottom_up_j := 0;
  while combination_sum_iv_bottom_up_j < n do begin
  if (combination_sum_iv_bottom_up_i - array_[combination_sum_iv_bottom_up_j]) >= 0 then begin
  combination_sum_iv_bottom_up_dp[combination_sum_iv_bottom_up_i] := combination_sum_iv_bottom_up_dp[combination_sum_iv_bottom_up_i] + combination_sum_iv_bottom_up_dp[combination_sum_iv_bottom_up_i - array_[combination_sum_iv_bottom_up_j]];
end;
  combination_sum_iv_bottom_up_j := combination_sum_iv_bottom_up_j + 1;
end;
  combination_sum_iv_bottom_up_i := combination_sum_iv_bottom_up_i + 1;
end;
  exit(combination_sum_iv_bottom_up_dp[target]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(combination_sum_iv([1, 2, 5], 5)));
  writeln(IntToStr(combination_sum_iv_dp_array([1, 2, 5], 5)));
  writeln(IntToStr(combination_sum_iv_bottom_up(3, [1, 2, 5], 5)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
