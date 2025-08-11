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
  waiting_times: IntArray;
  values: IntArray;
  burst_times: IntArray;
  x: real;
function calculate_waiting_times(burst_times: IntArray): IntArray; forward;
function calculate_turn_around_times(burst_times: IntArray; waiting_times: IntArray): IntArray; forward;
function mean(values: IntArray): real; forward;
function format_float_5(x: real): string; forward;
procedure main(); forward;
function calculate_waiting_times(burst_times: IntArray): IntArray;
var
  calculate_waiting_times_quantum: integer;
  calculate_waiting_times_rem: array of integer;
  calculate_waiting_times_i: integer;
  calculate_waiting_times_waiting: array of integer;
  calculate_waiting_times_t: integer;
  calculate_waiting_times_done: boolean;
  calculate_waiting_times_j: integer;
begin
  calculate_waiting_times_quantum := 2;
  calculate_waiting_times_rem := [];
  calculate_waiting_times_i := 0;
  while calculate_waiting_times_i < Length(burst_times) do begin
  calculate_waiting_times_rem := concat(calculate_waiting_times_rem, IntArray([burst_times[calculate_waiting_times_i]]));
  calculate_waiting_times_i := calculate_waiting_times_i + 1;
end;
  calculate_waiting_times_waiting := [];
  calculate_waiting_times_i := 0;
  while calculate_waiting_times_i < Length(burst_times) do begin
  calculate_waiting_times_waiting := concat(calculate_waiting_times_waiting, IntArray([0]));
  calculate_waiting_times_i := calculate_waiting_times_i + 1;
end;
  calculate_waiting_times_t := 0;
  while true do begin
  calculate_waiting_times_done := true;
  calculate_waiting_times_j := 0;
  while calculate_waiting_times_j < Length(burst_times) do begin
  if calculate_waiting_times_rem[calculate_waiting_times_j] > 0 then begin
  calculate_waiting_times_done := false;
  if calculate_waiting_times_rem[calculate_waiting_times_j] > calculate_waiting_times_quantum then begin
  calculate_waiting_times_t := calculate_waiting_times_t + calculate_waiting_times_quantum;
  calculate_waiting_times_rem[calculate_waiting_times_j] := calculate_waiting_times_rem[calculate_waiting_times_j] - calculate_waiting_times_quantum;
end else begin
  calculate_waiting_times_t := calculate_waiting_times_t + calculate_waiting_times_rem[calculate_waiting_times_j];
  calculate_waiting_times_waiting[calculate_waiting_times_j] := calculate_waiting_times_t - burst_times[calculate_waiting_times_j];
  calculate_waiting_times_rem[calculate_waiting_times_j] := 0;
end;
end;
  calculate_waiting_times_j := calculate_waiting_times_j + 1;
end;
  if calculate_waiting_times_done then begin
  exit(calculate_waiting_times_waiting);
end;
end;
  exit(calculate_waiting_times_waiting);
end;
function calculate_turn_around_times(burst_times: IntArray; waiting_times: IntArray): IntArray;
var
  calculate_turn_around_times_result_: array of integer;
  calculate_turn_around_times_i: integer;
begin
  calculate_turn_around_times_result_ := [];
  calculate_turn_around_times_i := 0;
  while calculate_turn_around_times_i < Length(burst_times) do begin
  calculate_turn_around_times_result_ := concat(calculate_turn_around_times_result_, IntArray([burst_times[calculate_turn_around_times_i] + waiting_times[calculate_turn_around_times_i]]));
  calculate_turn_around_times_i := calculate_turn_around_times_i + 1;
end;
  exit(calculate_turn_around_times_result_);
end;
function mean(values: IntArray): real;
var
  mean_total: integer;
  mean_i: integer;
begin
  mean_total := 0;
  mean_i := 0;
  while mean_i < Length(values) do begin
  mean_total := mean_total + values[mean_i];
  mean_i := mean_i + 1;
end;
  exit(Double(mean_total) / Double(Length(values)));
end;
function format_float_5(x: real): string;
var
  format_float_5_scaled: integer;
  format_float_5_int_part: integer;
  format_float_5_frac_part: integer;
  format_float_5_frac_str: string;
begin
  format_float_5_scaled := Trunc((x * 100000) + 0.5);
  format_float_5_int_part := format_float_5_scaled div 100000;
  format_float_5_frac_part := format_float_5_scaled mod 100000;
  format_float_5_frac_str := IntToStr(format_float_5_frac_part);
  while Length(format_float_5_frac_str) < 5 do begin
  format_float_5_frac_str := '0' + format_float_5_frac_str;
end;
  exit((IntToStr(format_float_5_int_part) + '.') + format_float_5_frac_str);
end;
procedure main();
var
  main_burst_times: array of integer;
  main_waiting_times: IntArray;
  main_turn_around_times: IntArray;
  main_i: integer;
  main_line: string;
begin
  main_burst_times := [3, 5, 7];
  main_waiting_times := calculate_waiting_times(main_burst_times);
  main_turn_around_times := calculate_turn_around_times(main_burst_times, main_waiting_times);
  writeln('Process ID 	Burst Time 	Waiting Time 	Turnaround Time');
  main_i := 0;
  while main_i < Length(main_burst_times) do begin
  main_line := (((((('  ' + IntToStr(main_i + 1)) + '		  ') + IntToStr(main_burst_times[main_i])) + '		  ') + IntToStr(main_waiting_times[main_i])) + '		  ') + IntToStr(main_turn_around_times[main_i]);
  writeln(main_line);
  main_i := main_i + 1;
end;
  writeln('');
  writeln('Average waiting time = ' + format_float_5(mean(main_waiting_times)));
  writeln('Average turn around time = ' + format_float_5(mean(main_turn_around_times)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
