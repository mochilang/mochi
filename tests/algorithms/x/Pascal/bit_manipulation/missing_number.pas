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
function min(xs: array of integer): integer;
var i, m: integer;
begin
  if Length(xs) = 0 then begin min := 0; exit; end;
  m := xs[0];
  for i := 1 to High(xs) do if xs[i] < m then m := xs[i];
  min := m;
end;
function max(xs: array of integer): integer;
var i, m: integer;
begin
  if Length(xs) = 0 then begin max := 0; exit; end;
  m := xs[0];
  for i := 1 to High(xs) do if xs[i] > m then m := xs[i];
  max := m;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  nums: IntArray;
function find_missing_number(nums: IntArray): integer; forward;
function find_missing_number(nums: IntArray): integer;
var
  find_missing_number_low: integer;
  find_missing_number_high: integer;
  find_missing_number_count: integer;
  find_missing_number_expected_sum: integer;
  find_missing_number_actual_sum: integer;
  find_missing_number_i: integer;
  find_missing_number_n: integer;
begin
  find_missing_number_low := Trunc(min(nums));
  find_missing_number_high := Trunc(max(nums));
  find_missing_number_count := (find_missing_number_high - find_missing_number_low) + 1;
  find_missing_number_expected_sum := ((find_missing_number_low + find_missing_number_high) * find_missing_number_count) div 2;
  find_missing_number_actual_sum := 0;
  find_missing_number_i := 0;
  find_missing_number_n := Length(nums);
  while find_missing_number_i < find_missing_number_n do begin
  find_missing_number_actual_sum := find_missing_number_actual_sum + nums[find_missing_number_i];
  find_missing_number_i := find_missing_number_i + 1;
end;
  exit(find_missing_number_expected_sum - find_missing_number_actual_sum);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(find_missing_number([0, 1, 3, 4]));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
