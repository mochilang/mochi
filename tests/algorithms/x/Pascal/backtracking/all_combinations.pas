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
  result_: array of IntArray;
function create_all_state(increment: integer; total: integer; level: integer; current: IntArray; result_: IntArrayArray): IntArrayArray; forward;
function generate_all_combinations(n: integer; k: integer): IntArrayArray; forward;
function create_all_state(increment: integer; total: integer; level: integer; current: IntArray; result_: IntArrayArray): IntArrayArray;
var
  create_all_state_i: integer;
  create_all_state_next_current: array of integer;
begin
  if level = 0 then begin
  exit(concat(result_, [current]));
end;
  create_all_state_i := increment;
  while create_all_state_i <= ((total - level) + 1) do begin
  create_all_state_next_current := concat(current, [create_all_state_i]);
  result_ := create_all_state(create_all_state_i + 1, total, level - 1, create_all_state_next_current, result_);
  create_all_state_i := create_all_state_i + 1;
end;
  exit(result_);
end;
function generate_all_combinations(n: integer; k: integer): IntArrayArray;
begin
  if (k < 0) or (n < 0) then begin
  exit([]);
end;
  result_ := [];
  exit(create_all_state(1, n, k, [], result_));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_list_int_to_str(generate_all_combinations(4, 2)));
  writeln(list_list_int_to_str(generate_all_combinations(3, 1)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
