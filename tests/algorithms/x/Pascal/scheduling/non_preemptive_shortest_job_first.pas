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
  no_of_processes: integer;
  burst_time: array of integer;
  arrival_time: array of integer;
  waiting_time: IntArray;
  turn_around_time: IntArray;
  i: integer;
  pid: integer;
  avg_wait: real;
  avg_turn: real;
  values: IntArray;
function calculate_waitingtime(arrival_time: IntArray; burst_time: IntArray; no_of_processes: integer): IntArray; forward;
function calculate_turnaroundtime(burst_time: IntArray; no_of_processes: integer; waiting_time: IntArray): IntArray; forward;
function average(values: IntArray): real; forward;
function calculate_waitingtime(arrival_time: IntArray; burst_time: IntArray; no_of_processes: integer): IntArray;
var
  calculate_waitingtime_waiting_time: array of integer;
  calculate_waitingtime_remaining_time: array of integer;
  calculate_waitingtime_i: integer;
  calculate_waitingtime_completed: integer;
  calculate_waitingtime_total_time: integer;
  calculate_waitingtime_ready_process: array of integer;
  calculate_waitingtime_target_process: integer;
  calculate_waitingtime_j: integer;
  calculate_waitingtime_k: integer;
  calculate_waitingtime_idx: integer;
begin
  calculate_waitingtime_i := 0;
  while calculate_waitingtime_i < no_of_processes do begin
  calculate_waitingtime_waiting_time := concat(calculate_waitingtime_waiting_time, IntArray([0]));
  calculate_waitingtime_remaining_time := concat(calculate_waitingtime_remaining_time, IntArray([burst_time[calculate_waitingtime_i]]));
  calculate_waitingtime_i := calculate_waitingtime_i + 1;
end;
  calculate_waitingtime_completed := 0;
  calculate_waitingtime_total_time := 0;
  while calculate_waitingtime_completed <> no_of_processes do begin
  calculate_waitingtime_ready_process := [];
  calculate_waitingtime_target_process := -1;
  calculate_waitingtime_j := 0;
  while calculate_waitingtime_j < no_of_processes do begin
  if (arrival_time[calculate_waitingtime_j] <= calculate_waitingtime_total_time) and (calculate_waitingtime_remaining_time[calculate_waitingtime_j] > 0) then begin
  calculate_waitingtime_ready_process := concat(calculate_waitingtime_ready_process, IntArray([calculate_waitingtime_j]));
end;
  calculate_waitingtime_j := calculate_waitingtime_j + 1;
end;
  if Length(calculate_waitingtime_ready_process) > 0 then begin
  calculate_waitingtime_target_process := calculate_waitingtime_ready_process[0];
  calculate_waitingtime_k := 0;
  while calculate_waitingtime_k < Length(calculate_waitingtime_ready_process) do begin
  calculate_waitingtime_idx := calculate_waitingtime_ready_process[calculate_waitingtime_k];
  if calculate_waitingtime_remaining_time[calculate_waitingtime_idx] < calculate_waitingtime_remaining_time[calculate_waitingtime_target_process] then begin
  calculate_waitingtime_target_process := calculate_waitingtime_idx;
end;
  calculate_waitingtime_k := calculate_waitingtime_k + 1;
end;
  calculate_waitingtime_total_time := calculate_waitingtime_total_time + burst_time[calculate_waitingtime_target_process];
  calculate_waitingtime_completed := calculate_waitingtime_completed + 1;
  calculate_waitingtime_remaining_time[calculate_waitingtime_target_process] := 0;
  calculate_waitingtime_waiting_time[calculate_waitingtime_target_process] := (calculate_waitingtime_total_time - arrival_time[calculate_waitingtime_target_process]) - burst_time[calculate_waitingtime_target_process];
end else begin
  calculate_waitingtime_total_time := calculate_waitingtime_total_time + 1;
end;
end;
  exit(calculate_waitingtime_waiting_time);
end;
function calculate_turnaroundtime(burst_time: IntArray; no_of_processes: integer; waiting_time: IntArray): IntArray;
var
  calculate_turnaroundtime_turn_around_time: array of integer;
  calculate_turnaroundtime_i: integer;
begin
  calculate_turnaroundtime_i := 0;
  while calculate_turnaroundtime_i < no_of_processes do begin
  calculate_turnaroundtime_turn_around_time := concat(calculate_turnaroundtime_turn_around_time, IntArray([burst_time[calculate_turnaroundtime_i] + waiting_time[calculate_turnaroundtime_i]]));
  calculate_turnaroundtime_i := calculate_turnaroundtime_i + 1;
end;
  exit(calculate_turnaroundtime_turn_around_time);
end;
function average(values: IntArray): real;
var
  average_total: integer;
  average_i: integer;
begin
  average_total := 0;
  average_i := 0;
  while average_i < Length(values) do begin
  average_total := average_total + values[average_i];
  average_i := average_i + 1;
end;
  exit(Double(average_total) / Double(Length(values)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln('[TEST CASE 01]');
  no_of_processes := 4;
  burst_time := [2, 5, 3, 7];
  arrival_time := [0, 0, 0, 0];
  waiting_time := calculate_waitingtime(arrival_time, burst_time, no_of_processes);
  turn_around_time := calculate_turnaroundtime(burst_time, no_of_processes, waiting_time);
  writeln('PID	Burst Time	Arrival Time	Waiting Time	Turnaround Time');
  i := 0;
  while i < no_of_processes do begin
  pid := i + 1;
  writeln((((((((IntToStr(pid) + '	') + IntToStr(burst_time[i])) + '			') + IntToStr(arrival_time[i])) + '				') + IntToStr(waiting_time[i])) + '				') + IntToStr(turn_around_time[i]));
  i := i + 1;
end;
  avg_wait := average(waiting_time);
  avg_turn := average(turn_around_time);
  writeln('' + #10 + 'Average waiting time = ' + FloatToStr(avg_wait));
  writeln('Average turnaround time = ' + FloatToStr(avg_turn));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
