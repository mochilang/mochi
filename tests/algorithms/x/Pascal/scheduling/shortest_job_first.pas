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
procedure show_list(xs: array of integer);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  waiting_time: IntArray;
  x: integer;
  arrival_time: IntArray;
  burst_time: IntArray;
  turn_around_time: IntArray;
  no_of_processes: integer;
function calculate_waitingtime(arrival_time: IntArray; burst_time: IntArray; no_of_processes: integer): IntArray; forward;
function calculate_turnaroundtime(burst_time: IntArray; no_of_processes: integer; waiting_time: IntArray): IntArray; forward;
function to_float(x: integer): real; forward;
procedure calculate_average_times(waiting_time: IntArray; turn_around_time: IntArray; no_of_processes: integer); forward;
function calculate_waitingtime(arrival_time: IntArray; burst_time: IntArray; no_of_processes: integer): IntArray;
var
  calculate_waitingtime_remaining_time: array of integer;
  calculate_waitingtime_i: integer;
  calculate_waitingtime_waiting_time: array of integer;
  calculate_waitingtime_complete: integer;
  calculate_waitingtime_increment_time: integer;
  calculate_waitingtime_minm: integer;
  calculate_waitingtime_short: integer;
  calculate_waitingtime_check: boolean;
  calculate_waitingtime_j: integer;
  calculate_waitingtime_finish_time: integer;
  calculate_waitingtime_finar: integer;
begin
  calculate_waitingtime_remaining_time := [];
  calculate_waitingtime_i := 0;
  while calculate_waitingtime_i < no_of_processes do begin
  calculate_waitingtime_remaining_time := concat(calculate_waitingtime_remaining_time, IntArray([burst_time[calculate_waitingtime_i]]));
  calculate_waitingtime_i := calculate_waitingtime_i + 1;
end;
  calculate_waitingtime_waiting_time := [];
  calculate_waitingtime_i := 0;
  while calculate_waitingtime_i < no_of_processes do begin
  calculate_waitingtime_waiting_time := concat(calculate_waitingtime_waiting_time, IntArray([0]));
  calculate_waitingtime_i := calculate_waitingtime_i + 1;
end;
  calculate_waitingtime_complete := 0;
  calculate_waitingtime_increment_time := 0;
  calculate_waitingtime_minm := 1000000000;
  calculate_waitingtime_short := 0;
  calculate_waitingtime_check := false;
  while calculate_waitingtime_complete <> no_of_processes do begin
  calculate_waitingtime_j := 0;
  while calculate_waitingtime_j < no_of_processes do begin
  if ((arrival_time[calculate_waitingtime_j] <= calculate_waitingtime_increment_time) and (calculate_waitingtime_remaining_time[calculate_waitingtime_j] > 0)) and (calculate_waitingtime_remaining_time[calculate_waitingtime_j] < calculate_waitingtime_minm) then begin
  calculate_waitingtime_minm := calculate_waitingtime_remaining_time[calculate_waitingtime_j];
  calculate_waitingtime_short := calculate_waitingtime_j;
  calculate_waitingtime_check := true;
end;
  calculate_waitingtime_j := calculate_waitingtime_j + 1;
end;
  if not calculate_waitingtime_check then begin
  calculate_waitingtime_increment_time := calculate_waitingtime_increment_time + 1;
  continue;
end;
  calculate_waitingtime_remaining_time[calculate_waitingtime_short] := calculate_waitingtime_remaining_time[calculate_waitingtime_short] - 1;
  calculate_waitingtime_minm := calculate_waitingtime_remaining_time[calculate_waitingtime_short];
  if calculate_waitingtime_minm = 0 then begin
  calculate_waitingtime_minm := 1000000000;
end;
  if calculate_waitingtime_remaining_time[calculate_waitingtime_short] = 0 then begin
  calculate_waitingtime_complete := calculate_waitingtime_complete + 1;
  calculate_waitingtime_check := false;
  calculate_waitingtime_finish_time := calculate_waitingtime_increment_time + 1;
  calculate_waitingtime_finar := calculate_waitingtime_finish_time - arrival_time[calculate_waitingtime_short];
  calculate_waitingtime_waiting_time[calculate_waitingtime_short] := calculate_waitingtime_finar - burst_time[calculate_waitingtime_short];
  if calculate_waitingtime_waiting_time[calculate_waitingtime_short] < 0 then begin
  calculate_waitingtime_waiting_time[calculate_waitingtime_short] := 0;
end;
end;
  calculate_waitingtime_increment_time := calculate_waitingtime_increment_time + 1;
end;
  exit(calculate_waitingtime_waiting_time);
end;
function calculate_turnaroundtime(burst_time: IntArray; no_of_processes: integer; waiting_time: IntArray): IntArray;
var
  calculate_turnaroundtime_turn_around_time: array of integer;
  calculate_turnaroundtime_i: integer;
begin
  calculate_turnaroundtime_turn_around_time := [];
  calculate_turnaroundtime_i := 0;
  while calculate_turnaroundtime_i < no_of_processes do begin
  calculate_turnaroundtime_turn_around_time := concat(calculate_turnaroundtime_turn_around_time, IntArray([burst_time[calculate_turnaroundtime_i] + waiting_time[calculate_turnaroundtime_i]]));
  calculate_turnaroundtime_i := calculate_turnaroundtime_i + 1;
end;
  exit(calculate_turnaroundtime_turn_around_time);
end;
function to_float(x: integer): real;
begin
  exit(x * 1);
end;
procedure calculate_average_times(waiting_time: IntArray; turn_around_time: IntArray; no_of_processes: integer);
var
  calculate_average_times_total_waiting_time: integer;
  calculate_average_times_total_turn_around_time: integer;
  calculate_average_times_i: integer;
  calculate_average_times_avg_wait: real;
  calculate_average_times_avg_turn: real;
begin
  calculate_average_times_total_waiting_time := 0;
  calculate_average_times_total_turn_around_time := 0;
  calculate_average_times_i := 0;
  while calculate_average_times_i < no_of_processes do begin
  calculate_average_times_total_waiting_time := calculate_average_times_total_waiting_time + waiting_time[calculate_average_times_i];
  calculate_average_times_total_turn_around_time := calculate_average_times_total_turn_around_time + turn_around_time[calculate_average_times_i];
  calculate_average_times_i := calculate_average_times_i + 1;
end;
  calculate_average_times_avg_wait := to_float(calculate_average_times_total_waiting_time) / to_float(no_of_processes);
  calculate_average_times_avg_turn := to_float(calculate_average_times_total_turn_around_time) / to_float(no_of_processes);
  writeln('Average waiting time = ' + FloatToStr(calculate_average_times_avg_wait));
  writeln('Average turn around time = ' + FloatToStr(calculate_average_times_avg_turn));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  show_list(calculate_waitingtime([1, 2, 3, 4], [3, 3, 5, 1], 4));
  show_list(calculate_waitingtime([1, 2, 3], [2, 5, 1], 3));
  show_list(calculate_waitingtime([2, 3], [5, 1], 2));
  show_list(calculate_turnaroundtime([3, 3, 5, 1], 4, [0, 3, 5, 0]));
  show_list(calculate_turnaroundtime([3, 3], 2, [0, 3]));
  show_list(calculate_turnaroundtime([8, 10, 1], 3, [1, 0, 3]));
  calculate_average_times([0, 3, 5, 0], [3, 6, 10, 1], 4);
  calculate_average_times([2, 3], [3, 6], 2);
  calculate_average_times([10, 4, 3], [2, 7, 6], 3);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
