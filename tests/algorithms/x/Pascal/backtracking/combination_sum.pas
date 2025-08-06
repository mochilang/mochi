{$mode objfpc}
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
  path: array of integer;
  result_: array of IntArray;
function backtrack(candidates: IntArray; start: integer; target: integer; path: IntArray; result_: IntArrayArray): IntArrayArray; forward;
function combination_sum(candidates: IntArray; target: integer): IntArrayArray; forward;
function backtrack(candidates: IntArray; start: integer; target: integer; path: IntArray; result_: IntArrayArray): IntArrayArray;
var
  backtrack_i: integer;
  backtrack_value: integer;
  backtrack_new_path: array of integer;
begin
  if target = 0 then begin
  exit(concat(result_, [path]));
end;
  backtrack_i := start;
  while backtrack_i < Length(candidates) do begin
  backtrack_value := candidates[backtrack_i];
  if backtrack_value <= target then begin
  backtrack_new_path := concat(path, [backtrack_value]);
  result_ := backtrack(candidates, backtrack_i, target - backtrack_value, backtrack_new_path, result_);
end;
  backtrack_i := backtrack_i + 1;
end;
  exit(result_);
end;
function combination_sum(candidates: IntArray; target: integer): IntArrayArray;
begin
  path := [];
  result_ := [];
  exit(backtrack(candidates, 0, target, path, result_));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_list_int_to_str(combination_sum([2, 3, 5], 8)));
  writeln(list_list_int_to_str(combination_sum([2, 3, 6, 7], 7)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
