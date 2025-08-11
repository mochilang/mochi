{$mode objfpc}
program Main;
uses SysUtils;
type Job = record
  id: integer;
  deadline: integer;
  profit: integer;
end;
type JobArray = array of Job;
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
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
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
  jobs1: array of Job;
  jobs2: array of Job;
  jobs: JobArray;
function makeJob(id: integer; deadline: integer; profit: integer): Job; forward;
function sort_jobs_by_profit(jobs: JobArray): JobArray; forward;
function max_deadline(jobs: JobArray): integer; forward;
function job_sequencing_with_deadlines(jobs: JobArray): IntArray; forward;
function makeJob(id: integer; deadline: integer; profit: integer): Job;
begin
  Result.id := id;
  Result.deadline := deadline;
  Result.profit := profit;
end;
function sort_jobs_by_profit(jobs: JobArray): JobArray;
var
  sort_jobs_by_profit_js: array of Job;
  sort_jobs_by_profit_i: integer;
  sort_jobs_by_profit_j: integer;
  sort_jobs_by_profit_a: Job;
  sort_jobs_by_profit_b: Job;
begin
  sort_jobs_by_profit_js := jobs;
  sort_jobs_by_profit_i := 0;
  while sort_jobs_by_profit_i < Length(sort_jobs_by_profit_js) do begin
  sort_jobs_by_profit_j := 0;
  while sort_jobs_by_profit_j < ((Length(sort_jobs_by_profit_js) - sort_jobs_by_profit_i) - 1) do begin
  sort_jobs_by_profit_a := sort_jobs_by_profit_js[sort_jobs_by_profit_j];
  sort_jobs_by_profit_b := sort_jobs_by_profit_js[sort_jobs_by_profit_j + 1];
  if sort_jobs_by_profit_a.profit < sort_jobs_by_profit_b.profit then begin
  sort_jobs_by_profit_js[sort_jobs_by_profit_j] := sort_jobs_by_profit_b;
  sort_jobs_by_profit_js[sort_jobs_by_profit_j + 1] := sort_jobs_by_profit_a;
end;
  sort_jobs_by_profit_j := sort_jobs_by_profit_j + 1;
end;
  sort_jobs_by_profit_i := sort_jobs_by_profit_i + 1;
end;
  exit(sort_jobs_by_profit_js);
end;
function max_deadline(jobs: JobArray): integer;
var
  max_deadline_max_d: integer;
  max_deadline_i: integer;
  max_deadline_job_var: Job;
  max_deadline_d: integer;
begin
  max_deadline_max_d := 0;
  max_deadline_i := 0;
  while max_deadline_i < Length(jobs) do begin
  max_deadline_job_var := jobs[max_deadline_i];
  max_deadline_d := max_deadline_job_var.deadline;
  if max_deadline_d > max_deadline_max_d then begin
  max_deadline_max_d := max_deadline_d;
end;
  max_deadline_i := max_deadline_i + 1;
end;
  exit(max_deadline_max_d);
end;
function job_sequencing_with_deadlines(jobs: JobArray): IntArray;
var
  job_sequencing_with_deadlines_js: JobArray;
  job_sequencing_with_deadlines_max_d: integer;
  job_sequencing_with_deadlines_time_slots: array of integer;
  job_sequencing_with_deadlines_t: integer;
  job_sequencing_with_deadlines_count: integer;
  job_sequencing_with_deadlines_max_profit: integer;
  job_sequencing_with_deadlines_i: integer;
  job_sequencing_with_deadlines_job_var: Job;
  job_sequencing_with_deadlines_j: integer;
  job_sequencing_with_deadlines_result_: array of integer;
begin
  job_sequencing_with_deadlines_js := sort_jobs_by_profit(jobs);
  job_sequencing_with_deadlines_max_d := max_deadline(job_sequencing_with_deadlines_js);
  job_sequencing_with_deadlines_time_slots := [];
  job_sequencing_with_deadlines_t := 0;
  while job_sequencing_with_deadlines_t < job_sequencing_with_deadlines_max_d do begin
  job_sequencing_with_deadlines_time_slots := concat(job_sequencing_with_deadlines_time_slots, IntArray([0 - 1]));
  job_sequencing_with_deadlines_t := job_sequencing_with_deadlines_t + 1;
end;
  job_sequencing_with_deadlines_count := 0;
  job_sequencing_with_deadlines_max_profit := 0;
  job_sequencing_with_deadlines_i := 0;
  while job_sequencing_with_deadlines_i < Length(job_sequencing_with_deadlines_js) do begin
  job_sequencing_with_deadlines_job_var := job_sequencing_with_deadlines_js[job_sequencing_with_deadlines_i];
  job_sequencing_with_deadlines_j := job_sequencing_with_deadlines_job_var.deadline - 1;
  while job_sequencing_with_deadlines_j >= 0 do begin
  if job_sequencing_with_deadlines_time_slots[job_sequencing_with_deadlines_j] = (0 - 1) then begin
  job_sequencing_with_deadlines_time_slots[job_sequencing_with_deadlines_j] := job_sequencing_with_deadlines_job_var.id;
  job_sequencing_with_deadlines_count := job_sequencing_with_deadlines_count + 1;
  job_sequencing_with_deadlines_max_profit := job_sequencing_with_deadlines_max_profit + job_sequencing_with_deadlines_job_var.profit;
  break;
end;
  job_sequencing_with_deadlines_j := job_sequencing_with_deadlines_j - 1;
end;
  job_sequencing_with_deadlines_i := job_sequencing_with_deadlines_i + 1;
end;
  job_sequencing_with_deadlines_result_ := [];
  job_sequencing_with_deadlines_result_ := concat(job_sequencing_with_deadlines_result_, IntArray([job_sequencing_with_deadlines_count]));
  job_sequencing_with_deadlines_result_ := concat(job_sequencing_with_deadlines_result_, IntArray([job_sequencing_with_deadlines_max_profit]));
  exit(job_sequencing_with_deadlines_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  jobs1 := [];
  jobs1 := concat(jobs1, [makeJob(1, 4, 20)]);
  jobs1 := concat(jobs1, [makeJob(2, 1, 10)]);
  jobs1 := concat(jobs1, [makeJob(3, 1, 40)]);
  jobs1 := concat(jobs1, [makeJob(4, 1, 30)]);
  writeln(list_int_to_str(job_sequencing_with_deadlines(jobs1)));
  jobs2 := [];
  jobs2 := concat(jobs2, [makeJob(1, 2, 100)]);
  jobs2 := concat(jobs2, [makeJob(2, 1, 19)]);
  jobs2 := concat(jobs2, [makeJob(3, 2, 27)]);
  jobs2 := concat(jobs2, [makeJob(4, 1, 25)]);
  jobs2 := concat(jobs2, [makeJob(5, 1, 15)]);
  writeln(list_int_to_str(job_sequencing_with_deadlines(jobs2)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
