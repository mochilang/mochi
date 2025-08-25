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
function make_list(make_list_len: int64; make_list_value: int64): IntArray; forward;
function max_int(max_int_a: int64; max_int_b: int64): int64; forward;
function min_int(min_int_a: int64; min_int_b: int64): int64; forward;
function min3(min3_a: int64; min3_b: int64; min3_c: int64): int64; forward;
function minimum_tickets_cost(minimum_tickets_cost_days: IntArray; minimum_tickets_cost_costs: IntArray): int64; forward;
function make_list(make_list_len: int64; make_list_value: int64): IntArray;
var
  make_list_arr: array of int64;
  make_list_i: int64;
begin
  make_list_arr := [];
  make_list_i := 0;
  while make_list_i < make_list_len do begin
  make_list_arr := concat(make_list_arr, IntArray([make_list_value]));
  make_list_i := make_list_i + 1;
end;
  exit(make_list_arr);
end;
function max_int(max_int_a: int64; max_int_b: int64): int64;
begin
  if max_int_a > max_int_b then begin
  exit(max_int_a);
end else begin
  exit(max_int_b);
end;
end;
function min_int(min_int_a: int64; min_int_b: int64): int64;
begin
  if min_int_a < min_int_b then begin
  exit(min_int_a);
end else begin
  exit(min_int_b);
end;
end;
function min3(min3_a: int64; min3_b: int64; min3_c: int64): int64;
begin
  exit(min_int(min_int(min3_a, min3_b), min3_c));
end;
function minimum_tickets_cost(minimum_tickets_cost_days: IntArray; minimum_tickets_cost_costs: IntArray): int64;
var
  minimum_tickets_cost_last_day: int64;
  minimum_tickets_cost_dp: IntArray;
  minimum_tickets_cost_day_index: int64;
  minimum_tickets_cost_d: int64;
  minimum_tickets_cost_cost1: int64;
  minimum_tickets_cost_cost7: int64;
  minimum_tickets_cost_cost30: int64;
begin
  if Length(minimum_tickets_cost_days) = 0 then begin
  exit(0);
end;
  minimum_tickets_cost_last_day := minimum_tickets_cost_days[Length(minimum_tickets_cost_days) - 1];
  minimum_tickets_cost_dp := make_list(minimum_tickets_cost_last_day + 1, 0);
  minimum_tickets_cost_day_index := 0;
  minimum_tickets_cost_d := 1;
  while minimum_tickets_cost_d <= minimum_tickets_cost_last_day do begin
  if (minimum_tickets_cost_day_index < Length(minimum_tickets_cost_days)) and (minimum_tickets_cost_d = minimum_tickets_cost_days[minimum_tickets_cost_day_index]) then begin
  minimum_tickets_cost_cost1 := minimum_tickets_cost_dp[minimum_tickets_cost_d - 1] + minimum_tickets_cost_costs[0];
  minimum_tickets_cost_cost7 := minimum_tickets_cost_dp[max_int(0, minimum_tickets_cost_d - 7)] + minimum_tickets_cost_costs[1];
  minimum_tickets_cost_cost30 := minimum_tickets_cost_dp[max_int(0, minimum_tickets_cost_d - 30)] + minimum_tickets_cost_costs[2];
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
  writeln('');
end.
