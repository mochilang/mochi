{$mode objfpc}
program Main;
uses SysUtils;
type Process = record
  process_name: string;
  arrival_time: integer;
  stop_time: integer;
  burst_time: integer;
  waiting_time: integer;
  turnaround_time: integer;
end;
type MLFQ = record
  number_of_queues: integer;
  time_slices: array of integer;
  ready_queue: array of Process;
  current_time: integer;
  finish_queue: array of Process;
end;
type RRResult = record
  finished: array of Process;
  ready: array of Process;
end;
type StrArray = array of string;
type IntArray = array of integer;
type ProcessArray = array of Process;
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
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
function list_to_str(xs: array of string): string;
var i: integer;
begin
  Result := '#(' + sLineBreak;
  for i := 0 to High(xs) do begin
    Result := Result + '  ''' + xs[i] + '''.' + sLineBreak;
  end;
  Result := Result + ')';
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
  P1: Process;
  P2: Process;
  P3: Process;
  P4: Process;
  number_of_queues: integer;
  time_slices: array of integer;
  queue: array of Process;
  mlfq_var: MLFQ;
  finish_queue: ProcessArray;
  process_var: Process;
  ready_queue: ProcessArray;
  time_slice: integer;
  arrival: integer;
  burst: integer;
  name: string;
  current_time: integer;
  nqueues: integer;
function makeRRResult(finished: ProcessArray; ready: ProcessArray): RRResult; forward;
function makeMLFQ(number_of_queues: integer; time_slices: IntArray; ready_queue: ProcessArray; current_time: integer; finish_queue: ProcessArray): MLFQ; forward;
function makeProcess(process_name: string; arrival_time: integer; stop_time: integer; burst_time: integer; waiting_time: integer; turnaround_time: integer): Process; forward;
function make_process(name: string; arrival: integer; burst: integer): Process; forward;
function make_mlfq(nqueues: integer; time_slices: IntArray; queue: ProcessArray; current_time: integer): MLFQ; forward;
function calculate_sequence_of_finish_queue(mlfq_var: MLFQ): StrArray; forward;
function calculate_waiting_time(queue: ProcessArray): IntArray; forward;
function calculate_turnaround_time(queue: ProcessArray): IntArray; forward;
function calculate_completion_time(queue: ProcessArray): IntArray; forward;
function calculate_remaining_burst_time_of_processes(queue: ProcessArray): IntArray; forward;
function update_waiting_time(mlfq_var: MLFQ; process_var: Process): integer; forward;
function first_come_first_served(mlfq_var: MLFQ; ready_queue: ProcessArray): ProcessArray; forward;
function round_robin(mlfq_var: MLFQ; ready_queue: ProcessArray; time_slice: integer): RRResult; forward;
function multi_level_feedback_queue(mlfq_var: MLFQ): ProcessArray; forward;
function makeRRResult(finished: ProcessArray; ready: ProcessArray): RRResult;
begin
  Result.finished := finished;
  Result.ready := ready;
end;
function makeMLFQ(number_of_queues: integer; time_slices: IntArray; ready_queue: ProcessArray; current_time: integer; finish_queue: ProcessArray): MLFQ;
begin
  Result.number_of_queues := number_of_queues;
  Result.time_slices := time_slices;
  Result.ready_queue := ready_queue;
  Result.current_time := current_time;
  Result.finish_queue := finish_queue;
end;
function makeProcess(process_name: string; arrival_time: integer; stop_time: integer; burst_time: integer; waiting_time: integer; turnaround_time: integer): Process;
begin
  Result.process_name := process_name;
  Result.arrival_time := arrival_time;
  Result.stop_time := stop_time;
  Result.burst_time := burst_time;
  Result.waiting_time := waiting_time;
  Result.turnaround_time := turnaround_time;
end;
function make_process(name: string; arrival: integer; burst: integer): Process;
begin
  exit(makeProcess(name, arrival, arrival, burst, 0, 0));
end;
function make_mlfq(nqueues: integer; time_slices: IntArray; queue: ProcessArray; current_time: integer): MLFQ;
begin
  exit(makeMLFQ(nqueues, time_slices, queue, current_time, []));
end;
function calculate_sequence_of_finish_queue(mlfq_var: MLFQ): StrArray;
var
  calculate_sequence_of_finish_queue_seq: array of string;
  calculate_sequence_of_finish_queue_i: integer;
  calculate_sequence_of_finish_queue_p: Process;
begin
  calculate_sequence_of_finish_queue_seq := [];
  calculate_sequence_of_finish_queue_i := 0;
  while calculate_sequence_of_finish_queue_i < Length(mlfq_var.finish_queue) do begin
  calculate_sequence_of_finish_queue_p := mlfq_var.finish_queue[calculate_sequence_of_finish_queue_i];
  calculate_sequence_of_finish_queue_seq := concat(calculate_sequence_of_finish_queue_seq, StrArray([calculate_sequence_of_finish_queue_p.process_name]));
  calculate_sequence_of_finish_queue_i := calculate_sequence_of_finish_queue_i + 1;
end;
  exit(calculate_sequence_of_finish_queue_seq);
end;
function calculate_waiting_time(queue: ProcessArray): IntArray;
var
  calculate_waiting_time_times: array of integer;
  calculate_waiting_time_i: integer;
  calculate_waiting_time_p: Process;
begin
  calculate_waiting_time_times := [];
  calculate_waiting_time_i := 0;
  while calculate_waiting_time_i < Length(queue) do begin
  calculate_waiting_time_p := queue[calculate_waiting_time_i];
  calculate_waiting_time_times := concat(calculate_waiting_time_times, IntArray([calculate_waiting_time_p.waiting_time]));
  calculate_waiting_time_i := calculate_waiting_time_i + 1;
end;
  exit(calculate_waiting_time_times);
end;
function calculate_turnaround_time(queue: ProcessArray): IntArray;
var
  calculate_turnaround_time_times: array of integer;
  calculate_turnaround_time_i: integer;
  calculate_turnaround_time_p: Process;
begin
  calculate_turnaround_time_times := [];
  calculate_turnaround_time_i := 0;
  while calculate_turnaround_time_i < Length(queue) do begin
  calculate_turnaround_time_p := queue[calculate_turnaround_time_i];
  calculate_turnaround_time_times := concat(calculate_turnaround_time_times, IntArray([calculate_turnaround_time_p.turnaround_time]));
  calculate_turnaround_time_i := calculate_turnaround_time_i + 1;
end;
  exit(calculate_turnaround_time_times);
end;
function calculate_completion_time(queue: ProcessArray): IntArray;
var
  calculate_completion_time_times: array of integer;
  calculate_completion_time_i: integer;
  calculate_completion_time_p: Process;
begin
  calculate_completion_time_times := [];
  calculate_completion_time_i := 0;
  while calculate_completion_time_i < Length(queue) do begin
  calculate_completion_time_p := queue[calculate_completion_time_i];
  calculate_completion_time_times := concat(calculate_completion_time_times, IntArray([calculate_completion_time_p.stop_time]));
  calculate_completion_time_i := calculate_completion_time_i + 1;
end;
  exit(calculate_completion_time_times);
end;
function calculate_remaining_burst_time_of_processes(queue: ProcessArray): IntArray;
var
  calculate_remaining_burst_time_of_processes_times: array of integer;
  calculate_remaining_burst_time_of_processes_i: integer;
  calculate_remaining_burst_time_of_processes_p: Process;
begin
  calculate_remaining_burst_time_of_processes_times := [];
  calculate_remaining_burst_time_of_processes_i := 0;
  while calculate_remaining_burst_time_of_processes_i < Length(queue) do begin
  calculate_remaining_burst_time_of_processes_p := queue[calculate_remaining_burst_time_of_processes_i];
  calculate_remaining_burst_time_of_processes_times := concat(calculate_remaining_burst_time_of_processes_times, IntArray([calculate_remaining_burst_time_of_processes_p.burst_time]));
  calculate_remaining_burst_time_of_processes_i := calculate_remaining_burst_time_of_processes_i + 1;
end;
  exit(calculate_remaining_burst_time_of_processes_times);
end;
function update_waiting_time(mlfq_var: MLFQ; process_var: Process): integer;
begin
  process_var.waiting_time := process_var.waiting_time + (mlfq_var.current_time - process_var.stop_time);
  exit(process_var.waiting_time);
end;
function first_come_first_served(mlfq_var: MLFQ; ready_queue: ProcessArray): ProcessArray;
var
  first_come_first_served_finished: array of Process;
  first_come_first_served_rq: array of Process;
  first_come_first_served_cp: Process;
begin
  first_come_first_served_finished := [];
  first_come_first_served_rq := ready_queue;
  while Length(first_come_first_served_rq) <> 0 do begin
  first_come_first_served_cp := first_come_first_served_rq[0];
  first_come_first_served_rq := copy(first_come_first_served_rq, 1, (Length(first_come_first_served_rq) - (1)));
  if mlfq_var.current_time < first_come_first_served_cp.arrival_time then begin
  mlfq_var.current_time := first_come_first_served_cp.arrival_time;
end;
  update_waiting_time(mlfq_var, first_come_first_served_cp);
  mlfq_var.current_time := mlfq_var.current_time + first_come_first_served_cp.burst_time;
  first_come_first_served_cp.burst_time := 0;
  first_come_first_served_cp.turnaround_time := mlfq_var.current_time - first_come_first_served_cp.arrival_time;
  first_come_first_served_cp.stop_time := mlfq_var.current_time;
  first_come_first_served_finished := concat(first_come_first_served_finished, [first_come_first_served_cp]);
end;
  mlfq_var.finish_queue := concat(mlfq_var.finish_queue, first_come_first_served_finished);
  exit(first_come_first_served_finished);
end;
function round_robin(mlfq_var: MLFQ; ready_queue: ProcessArray; time_slice: integer): RRResult;
var
  round_robin_finished: array of Process;
  round_robin_rq: array of Process;
  round_robin_count: integer;
  round_robin_i: integer;
  round_robin_cp: Process;
begin
  round_robin_finished := [];
  round_robin_rq := ready_queue;
  round_robin_count := Length(round_robin_rq);
  round_robin_i := 0;
  while round_robin_i < round_robin_count do begin
  round_robin_cp := round_robin_rq[0];
  round_robin_rq := copy(round_robin_rq, 1, (Length(round_robin_rq) - (1)));
  if mlfq_var.current_time < round_robin_cp.arrival_time then begin
  mlfq_var.current_time := round_robin_cp.arrival_time;
end;
  update_waiting_time(mlfq_var, round_robin_cp);
  if round_robin_cp.burst_time > time_slice then begin
  mlfq_var.current_time := mlfq_var.current_time + time_slice;
  round_robin_cp.burst_time := round_robin_cp.burst_time - time_slice;
  round_robin_cp.stop_time := mlfq_var.current_time;
  round_robin_rq := concat(round_robin_rq, [round_robin_cp]);
end else begin
  mlfq_var.current_time := mlfq_var.current_time + round_robin_cp.burst_time;
  round_robin_cp.burst_time := 0;
  round_robin_cp.stop_time := mlfq_var.current_time;
  round_robin_cp.turnaround_time := mlfq_var.current_time - round_robin_cp.arrival_time;
  round_robin_finished := concat(round_robin_finished, [round_robin_cp]);
end;
  round_robin_i := round_robin_i + 1;
end;
  mlfq_var.finish_queue := concat(mlfq_var.finish_queue, round_robin_finished);
  exit(makeRRResult(round_robin_finished, round_robin_rq));
end;
function multi_level_feedback_queue(mlfq_var: MLFQ): ProcessArray;
var
  multi_level_feedback_queue_i: integer;
  multi_level_feedback_queue_rr: RRResult;
begin
  multi_level_feedback_queue_i := 0;
  while multi_level_feedback_queue_i < (mlfq_var.number_of_queues - 1) do begin
  multi_level_feedback_queue_rr := round_robin(mlfq_var, mlfq_var.ready_queue, mlfq_var.time_slices[multi_level_feedback_queue_i]);
  mlfq_var.ready_queue := multi_level_feedback_queue_rr.ready;
  multi_level_feedback_queue_i := multi_level_feedback_queue_i + 1;
end;
  first_come_first_served(mlfq_var, mlfq_var.ready_queue);
  exit(mlfq_var.finish_queue);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  P1 := make_process('P1', 0, 53);
  P2 := make_process('P2', 0, 17);
  P3 := make_process('P3', 0, 68);
  P4 := make_process('P4', 0, 24);
  number_of_queues := 3;
  time_slices := [17, 25];
  queue := [P1, P2, P3, P4];
  mlfq_var := make_mlfq(number_of_queues, time_slices, queue, 0);
  finish_queue := multi_level_feedback_queue(mlfq_var);
  writeln('waiting time:			' + list_int_to_str(calculate_waiting_time([P1, P2, P3, P4])));
  writeln('completion time:		' + list_int_to_str(calculate_completion_time([P1, P2, P3, P4])));
  writeln('turnaround time:		' + list_int_to_str(calculate_turnaround_time([P1, P2, P3, P4])));
  writeln('sequence of finished processes:	' + list_to_str(calculate_sequence_of_finish_queue(mlfq_var)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
