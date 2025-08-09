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
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
function contains(xs: array of integer; v: integer): boolean;
var i: integer;
begin
  for i := 0 to High(xs) do begin
    if xs[i] = v then begin
      contains := true; exit;
    end;
  end;
  contains := false;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  task_performed: IntArrayArray;
  used: IntArray;
  person: integer;
function count_assignments(person: integer; task_performed: IntArrayArray; used: IntArray): integer; forward;
function count_no_of_ways(task_performed: IntArrayArray): integer; forward;
procedure main(); forward;
function count_assignments(person: integer; task_performed: IntArrayArray; used: IntArray): integer;
var
  count_assignments_total: integer;
  count_assignments_tasks: array of integer;
  count_assignments_i: integer;
  count_assignments_t: integer;
begin
  if person = Length(task_performed) then begin
  exit(1);
end;
  count_assignments_total := 0;
  count_assignments_tasks := task_performed[person];
  count_assignments_i := 0;
  while count_assignments_i < Length(count_assignments_tasks) do begin
  count_assignments_t := count_assignments_tasks[count_assignments_i];
  if not (contains(used, count_assignments_t)) then begin
  count_assignments_total := count_assignments_total + count_assignments(person + 1, task_performed, concat(used, IntArray([count_assignments_t])));
end;
  count_assignments_i := count_assignments_i + 1;
end;
  exit(count_assignments_total);
end;
function count_no_of_ways(task_performed: IntArrayArray): integer;
begin
  exit(count_assignments(0, task_performed, []));
end;
procedure main();
var
  main_task_performed: array of IntArray;
begin
  main_task_performed := [[1, 3, 4], [1, 2, 5], [3, 4]];
  writeln(IntToStr(count_no_of_ways(main_task_performed)));
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
