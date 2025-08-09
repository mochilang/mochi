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
  len: integer;
  value: integer;
  a: integer;
  b: integer;
  c: integer;
  costs: IntArray;
  days: IntArray;
function make_list(len: integer; value: integer): IntArray; forward;
function max_int(a: integer; b: integer): integer; forward;
function min_int(a: integer; b: integer): integer; forward;
function min3(a: integer; b: integer; c: integer): integer; forward;
function minimum_tickets_cost(days: IntArray; costs: IntArray): integer; forward;
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
function max_int(a: integer; b: integer): integer;
begin
  if a > b then begin
  exit(a);
end else begin
  exit(b);
end;
end;
function min_int(a: integer; b: integer): integer;
begin
  if a < b then begin
  exit(a);
end else begin
  exit(b);
end;
end;
function min3(a: integer; b: integer; c: integer): integer;
begin
  exit(min_int(min_int(a, b), c));
end;
function minimum_tickets_cost(days: IntArray; costs: IntArray): integer;
var
  minimum_tickets_cost_last_day: integer;
  minimum_tickets_cost_dp: IntArray;
  minimum_tickets_cost_day_index: integer;
  minimum_tickets_cost_d: integer;
  minimum_tickets_cost_cost1: integer;
  minimum_tickets_cost_cost7: integer;
  minimum_tickets_cost_cost30: integer;
begin
  if Length(days) = 0 then begin
  exit(0);
end;
  minimum_tickets_cost_last_day := days[Length(days) - 1];
  minimum_tickets_cost_dp := make_list(minimum_tickets_cost_last_day + 1, 0);
  minimum_tickets_cost_day_index := 0;
  minimum_tickets_cost_d := 1;
  while minimum_tickets_cost_d <= minimum_tickets_cost_last_day do begin
  if (minimum_tickets_cost_day_index < Length(days)) and (minimum_tickets_cost_d = days[minimum_tickets_cost_day_index]) then begin
  minimum_tickets_cost_cost1 := minimum_tickets_cost_dp[minimum_tickets_cost_d - 1] + costs[0];
  minimum_tickets_cost_cost7 := minimum_tickets_cost_dp[max_int(0, minimum_tickets_cost_d - 7)] + costs[1];
  minimum_tickets_cost_cost30 := minimum_tickets_cost_dp[max_int(0, minimum_tickets_cost_d - 30)] + costs[2];
  minimum_tickets_cost_dp[minimum_tickets_cost_d] := min3(minimum_tickets_cost_cost1, minimum_tickets_cost_cost7, minimum_tickets_cost_cost30);
  minimum_tickets_cost_day_index := minimum_tickets_cost_day_index + 1;
end else begin
  minimum_tickets_cost_dp[minimum_tickets_cost_d] := minimum_tickets_cost_dp[minimum_tickets_cost_d - 1];
end;
  minimum_tickets_cost_d := minimum_tickets_cost_d + 1;
end;
  exit(minimum_tickets_cost_dp[minimum_tickets_cost_last_day]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(minimum_tickets_cost([1, 4, 6, 7, 8, 20], [2, 7, 15])));
  writeln(IntToStr(minimum_tickets_cost([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 31], [2, 7, 15])));
  writeln(IntToStr(minimum_tickets_cost([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 31], [2, 90, 150])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
